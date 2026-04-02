{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Transform.Sub
  ( Prev (..),
    substituteTemplate,
  )
where

import Control.Lens hiding (Const)
import Control.Lens.Extras
import Data.Data
import Data.List
import Data.Loc (noLoc)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)
import Language.C hiding (mkIdent)
import Language.C.Quote.C
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class
import Text.Show.Pretty (pPrint)
import Transform.Build
import Transform.Common
import qualified Data.Set as S

data Prev = Prev {prevSpec :: Maybe StateSpec, prevSF :: [StateField]}

substituteTemplate :: [Definition] -> StateSpec -> Prev -> (Prev, String -> String)
substituteTemplate from spec Prev {..} =
  let render x = pretty 120 $ ppr x
      Just (Bodies {..}) = getBodies spec prevSpec from
      renderDecls xs = concatMap ((++ ";\n") . render) xs
      mergedSF = mergeSF prevSF (fields spec)
      withTemplate =
        lined %~ \case
          "//DECLS" -> render (mapMaybe toDecl from)
          "//DEFS" -> render (dropMainNonStatic spec from)
          "//STRUCTBODY" -> renderDecls $ buildStateMembers $ uniqueDummy mergedSF
          "//PREVSTRUCTBODY" -> renderDecls $ buildStateMembers $ uniqueDummy prevSF
          "//REINITALLOCBODY_ALLOC" -> render reinitAllocBody
          "//REINITINPLACEBODY" -> render reinitInPlaceBody
          "//INITBODY" -> render initBody
          "//STEPBODY" -> render stepBody
          "//UNINITBODY" -> render uninitBody
          x -> x
   in (Prev {prevSpec = Just spec, prevSF = mergedSF}, withTemplate)

mergeSF :: [StateField] -> [StateField] -> [StateField]
mergeSF old new = trimTrailingDummies (reused ++ remaining)
  where
    oldDeclSet = S.fromList oldDecl
    oldDecl = map show $ buildStateMembers old
    newDecl = map show $ buildStateMembers new
    (sameDecl, notFound) = zip newDecl new & partition ((`S.member` oldDeclSet) . fst)
    sameDeclSet = S.fromList $ map fst sameDecl
    expandedDummies = zipWith dummyWhenMissing old oldDecl
    (reused, remaining) = reuseDummies expandedDummies (map snd notFound)
    oldFieldMap =
      M.fromList
        [ ((fieldOrigName f, fieldScope f), f)
          | f <- old
        ]

    dummyWhenMissing o@StateField {..} oStr
      | oStr `S.member` sameDeclSet = o
      | otherwise = StateField {fieldName = "", fieldInit = Nothing, fieldScope = Nothing, fieldMoved = False, ..}

    reuseDummies fields newFields = foldl' reuse (fields, []) newFields
      where
        reuse (fieldsAcc, acc) newField =
          case bestDummyIndex newField fieldsAcc of
            Just idx -> (replaceAt idx (markMoved newField) fieldsAcc, acc)
            Nothing -> (fieldsAcc, acc ++ [newField])

    lookupPrevField field = M.lookup (fieldOrigName field, fieldScope field) oldFieldMap

    markMoved field =
      case lookupPrevField field of
        Just _ -> field {fieldMoved = True}
        Nothing -> field

    bestDummyIndex newField fields =
      let lastRealIndex = lastNonDummyIndex fields
          candidates =
            [ (idx, scoreDummy newField field)
              | (idx, field) <- zip [0 ..] fields,
                fieldName field == "",
                fitsInDummy newField field,
                idx > lastRealIndex || exactMatch newField field
            ]
       in case candidates of
            [] -> Nothing
            _ -> Just $ fst $ minimumBy (comparing snd) candidates

    lastNonDummyIndex fields =
      case [idx | (idx, field) <- zip [0 ..] fields, fieldName field /= ""] of
        [] -> -1
        xs -> last xs

    exactMatch newField field =
      sameTypeRank && sameSize
      where
        sameTypeRank = sameTypeAndRank newField field
        sameSize =
          case (mapM constToArrayLen (fieldArraySize field), mapM constToArrayLen (fieldArraySize newField)) of
            (Just ds, Just ns) -> ds == ns
            _ -> False

    fitsInDummy newField field =
      sameTypeAndRank newField field && sizeFits
      where
        sizeFits =
          case (mapM constToArrayLen (fieldArraySize field), mapM constToArrayLen (fieldArraySize newField)) of
            (Just ds, Just ns) | length ds == length ns -> and (zipWith (>=) ds ns)
            _ -> False

    sameTypeAndRank newField field =
      show (fieldType field) == show (fieldType newField)
        && length (fieldArraySize field) == length (fieldArraySize newField)

    scoreDummy newField field =
      let exactSizePenalty = if exactMatch newField field then 0 :: Int else 1
          sizePenalty = case (mapM constToArrayLen (fieldArraySize field), mapM constToArrayLen (fieldArraySize newField)) of
            (Just ds, Just ns) | length ds == length ns -> sum (zipWith (\a b -> abs (a - b)) ds ns)
            _ -> 1000000
       in (exactSizePenalty, sizePenalty)

    replaceAt idx newField fields =
      [ if i == idx then newField else field
        | (i, field) <- zip [0 ..] fields
      ]

    trimTrailingDummies = reverse . dropWhile ((== "") . fieldName) . reverse

toDecl :: Definition -> Maybe Definition
toDecl (FuncDef f s) | f ^. funcName /= "main" = Just $ DecDef (InitGroup (f ^. funcDs) [] [Init (f ^. funcId) proto Nothing Nothing [] noLoc] noLoc) noLoc
  where
    proto = Proto (DeclRoot noLoc) (f ^. funcParams) noLoc
toDecl td@(DecDef (TypedefGroup {}) _) = Just td
toDecl _ = Nothing

funcParams :: Lens' Func Params
funcParams op (Func a b c d e f) = op d <&> \b' -> Func a b c d e f
funcParams op (OldFunc a b c [] e f g) = op (Params [] False noLoc) <&> \(Params ps _ _) -> OldFunc a b c (getId ps) e f g

getId :: [Param] -> [Id]
getId = map (\ (Param (Just a) _ _ _) -> a)

funcName :: Lens' Func String
funcName op (Func a (Id b c) d e f g) = op b <&> \b' -> Func a (Id b' c) d e f g
funcName op (OldFunc a (Id b c) d e f g h) = op b <&> \b' -> OldFunc a (Id b' c) d e f g h

funcDs :: Lens' Func DeclSpec
funcDs op (Func a b c d e f) = op a <&> \a' -> Func a' b c d e f
funcDs op (OldFunc a b c d e f g) = op a <&> \a' -> OldFunc a' b c d e f g

funcId :: Lens' Func Id
funcId op (Func a b c d e f) = op b <&> \b' -> Func a b' c d e f
funcId op (OldFunc a b c d e f g) = op b <&> \b' -> OldFunc a b' c d e f g

-- could be Data.Data.Lens.template?
funcBlockItems :: Lens' Func [BlockItem]
funcBlockItems op (Func a b c d bs f) = op bs <&> \bs' -> Func a b c d bs' f
funcBlockItems op (OldFunc a b c d e bs f) = op bs <&> \bs' -> OldFunc a b c d e bs' f

--------------------------------------------------------------------------------
-- Bodies helpers
--------------------------------------------------------------------------------

data Bodies = Bodies {initBody, stepBody, reinitAllocBody, reinitInPlaceBody, uninitBody :: Stm}
  deriving (Data, Show)

getBodies :: StateSpec -> Maybe StateSpec -> [Definition] -> Maybe Bodies
getBodies spec prevSpec decls = listToMaybe $ mapMaybe splitMainDef decls
  where
    splitMainDef (FuncDef (Fun "main" items) _) = withItems items
    splitMainDef _ = Nothing

    withItems items =
      let SplitOnLastWhile {..} = splitOnLastWhile items
          (initItems, updatePrefix) = partition keepInitItem preItems
          initItems' = initItems <> map BlockStm (initStmts spec)
          stepItems =
            updatePrefix
              <> loopBodyItems
              <> maybe [] (\cond -> [BlockStm (Return (Just cond) noLoc)]) loopCond
          initBody = Block initItems' noLoc
          stepBody = Block stepItems noLoc
          uninitBody = Block postItems noLoc
          reinitAllocBody = Block (map BlockStm (reinitAllocStmts prevSpec spec)) noLoc
          reinitInPlaceBody = Block (map BlockStm (reinitInPlaceStmts prevSpec spec)) noLoc
          structBody = buildStateMembers (fields spec)
       in Just (Bodies {..}) & applyRewrites (useRewrite spec (mkIdent "s"))

    keepInitItem item =
      let names = map identName (toListOf biplate item)
       in all keepInit names

data SplitOnLastWhile = SplitOnLastWhile {loopCond :: Maybe Exp, preItems, loopBodyItems, postItems :: [BlockItem]}

splitOnLastWhile :: [BlockItem] -> SplitOnLastWhile
splitOnLastWhile items =
  break isWhile (reverse items)
    & \case
      (a, (BlockStm (While cond body _)) : cs) -> SplitOnLastWhile (Just cond) (reverse cs) (compoundItems body) (reverse a)
      (a, cs) -> SplitOnLastWhile Nothing (reverse cs) [] (reverse a)
  where
    compoundItems (Block items _) = items
    compoundItems stmt = [BlockStm stmt]

-- | reinitAllocStmts s t is the body of ReinitAlloc(prevstate *s, state *t)
-- s previous, t new
reinitAllocStmts :: Maybe StateSpec -> StateSpec -> [Stm]
reinitAllocStmts Nothing _ = []
reinitAllocStmts (Just prevSpec) spec =
  concatMap reinitFieldToNew (nonDummyFields spec)
  where
    prevMap =
      M.fromList
        [ ((fieldOrigName f, fieldScope f), f)
          | f <- fields prevSpec
        ]

    lookupPrevField f = M.lookup (fieldOrigName f, fieldScope f) prevMap

    reinitFieldToNew field =
      case lookupPrevField field of
        Just prevField -> copyFieldToNew prevField field
        _ -> initTarget "t" field (fieldInit field)

-- | reinitInPlaceStmts s is the body of ReinitInPlace(prevstate *s)
-- s previous, t new (same buffer)
reinitInPlaceStmts :: Maybe StateSpec -> StateSpec -> [Stm]
reinitInPlaceStmts Nothing _ = []
reinitInPlaceStmts (Just prevSpec) spec =
  concatMap reinitFieldInPlace (nonDummyFields spec)
  where
    prevMap =
      M.fromList
        [ ((fieldOrigName f, fieldScope f), f)
          | f <- fields prevSpec
        ]

    lookupPrevField f = M.lookup (fieldOrigName f, fieldScope f) prevMap

    reinitFieldInPlace field =
      case lookupPrevField field of
        Nothing -> initTarget "t" field (fieldInit field)
        Just prevField
          | fieldMoved field || arraySizeChanged field prevField -> copyFieldInPlace prevField field
          | initEqual (fieldInit field) (fieldInit prevField) -> []
          | otherwise -> initTarget "t" field (fieldInit field)

initTarget :: String -> StateField -> Maybe Initializer -> [Stm]
initTarget _ _ Nothing = []
initTarget target StateField {..} (Just initVal) =
  case mapM constToArrayLen fieldArraySize of
    Just indexBounds -> [genLoopsST fieldName indexBounds]
    Nothing -> [[cstm| $id:target->$id:fieldName = $(initToExpr fieldType initVal); |]]

copyFieldToNew :: StateField -> StateField -> [Stm]
copyFieldToNew prevField field =
  case ( mapM constToArrayLen $ fieldArraySize field,
         mapM constToArrayLen $ fieldArraySize prevField
       ) of
    (Just newDims, Just prevDims) ->
      let minDims = zipWith min newDims prevDims
          copyStmt =
            genLoops minDims $ \vs ->
              let lhs = indexExpr [cexp| t->$id:(fieldName field) |] vs
                  rhs = indexExpr [cexp| s->$id:(fieldName prevField) |] vs
               in [cstm| $exp:lhs = $exp:rhs; |]
       in [copyStmt]
    (Nothing, Nothing) -> [[cstm| t->$id:(fieldName field) = s->$id:(fieldName prevField); |]]
    _ -> initTarget "t" field (fieldInit field)

copyFieldInPlace :: StateField -> StateField -> [Stm]
copyFieldInPlace prevField field =
  case ( mapM constToArrayLen $ fieldArraySize field,
         mapM constToArrayLen $ fieldArraySize prevField
       ) of
    (Just newDims, Just prevDims) ->
      let minDims = zipWith min newDims prevDims
          copyStmt =
            genLoops minDims $ \vs ->
              let lhs = indexExpr [cexp| t->$id:(fieldName field) |] vs
                  rhs = indexExpr [cexp| s->$id:(fieldName prevField) |] vs
               in [cstm| $exp:lhs = $exp:rhs; |]
          zeroStmt =
            if or (zipWith (>) newDims prevDims)
              then
                Just
                  [cstm|
                    if (sizeof(t->$id:(fieldName field)) > sizeof(s->$id:(fieldName prevField))) {
                        memset(
                          (void *)((char *)t->$id:(fieldName field) + sizeof(s->$id:(fieldName prevField))),
                          0,
                          sizeof(t->$id:(fieldName field)) - sizeof(s->$id:(fieldName prevField))
                        );
                    }
                  |]
              else Nothing
       in maybe [copyStmt] (\stmt -> [stmt, copyStmt]) zeroStmt
    (Nothing, Nothing) -> [[cstm| t->$id:(fieldName field) = s->$id:(fieldName prevField); |]]
    _ -> initTarget "t" field (fieldInit field)

nonDummyFields :: StateSpec -> [StateField]
nonDummyFields spec = filter ((/= "") . fieldName) (fields spec)

arraySizeChanged :: StateField -> StateField -> Bool
arraySizeChanged field prevField =
  case (mapM constToArrayLen $ fieldArraySize field, mapM constToArrayLen $ fieldArraySize prevField) of
    (Nothing, Nothing) -> False
    (Just ns, Just ps) -> ns /= ps
    _ -> True

initEqual :: Maybe Initializer -> Maybe Initializer -> Bool
initEqual Nothing Nothing = True
initEqual (Just a) (Just b) = not (cinitNE a b)
initEqual _ _ = False

cinitNE :: Initializer -> Initializer -> Bool
cinitNE (ExpInitializer (Const a _) _) (ExpInitializer (Const b _) _) = not (cinitEQ a b)
cinitNE _ _ = False

cinitEQ :: Const -> Const -> Bool
cinitEQ (StringConst s _ _) (StringConst t _ _) = s == t
cinitEQ (CharConst s _ _) (CharConst t _ _) = s == t
cinitEQ (IntConst s _ _ _) (IntConst t _ _ _) = s == t
cinitEQ (FloatConst s _ _) (FloatConst t _ _) = s == t
cinitEQ _ _ = False

keepInit :: String -> Bool
keepInit _ = True

test = do
  let fn = [cunit| void f() { while (true) { static int array[10]; } } |]
      spec = buildStateSpec fn
      mergedSF = maybe id (mergeSF . fields) Nothing (fields spec)
  pPrint fn
  pPrint $ map (pretty 100 . ppr) $ buildStateMembers mergedSF

  putStrLn $ pretty 120 $ ppr $ genLoopsST "x" [1, 5]

test2 = do
  let dl =
        [cunit| 
        typedef struct { float x, y; } Vector2;
        typedef struct {
                        Vector2 a, b;
                        typename bool placed[2];
                      } Seg;
        Seg segs[2]; 
        void f(void);
        void f(void) { printf(); }

        |]
  pPrint dl

  -- let ss0 = StateSpec [] [] (const []) []
  pPrint $ mapMaybe toDecl dl

test3 = do
  let a = [cunit| float xs[2]; char n; |]
  let b = [cunit| float xs[3]; char n; |]
  let c = [cunit| float xs[4]; char n; |]
  let d = [cunit| float xs[3]; char n; |]
  let e = [cunit| float xs[2]; char n; |]

  let sa = buildStateSpec a
  let sb = buildStateSpec b
  let sc = buildStateSpec c
  let sd = buildStateSpec d
  let se = buildStateSpec e

  let fa = fields sa
  let fb = mergeSF (fields sa) (fields sb)
  let fc = fb `mergeSF` fields sc
  let fd = fc `mergeSF` fields sd
  let fe = fd `mergeSF` fields se
  let renderDecls xs = concatMap ((++ ";\n") . render) xs
      render x = pretty 120 $ ppr x
      pp = putStrLn . renderDecls . buildStateMembers . uniqueDummy
  putStrLn "struct state_a {"
  pp fa
  putStrLn "} struct state_b {"
  pp fb
  putStrLn "} struct state_c {"
  pp fc
  putStrLn "} struct state_d {"
  pp fd
  putStrLn "} struct state_e {"
  pp fe
  putStrLn "}"

test4 = do
  let a = [cunit| float xs[2]; char n; |]
  let b = [cunit| float xs[3]; char n; |]
  let c = [cunit| float xs[4]; char n; |]
  let d = [cunit| float xs[3]; char n; |]
  let e = [cunit| float xs[2]; char n; |]

  let sa = buildStateSpec a
  let sb = buildStateSpec b
  let sc = buildStateSpec c
  let sd = buildStateSpec d
  let se = buildStateSpec e

  let fa = fields sa
  let fb = mergeSF (fields sa) (fields sb)
  let fc = fb `mergeSF` fields sc
  let fd = fc `mergeSF` fields sd
  let fe = fd `mergeSF` fields se

  let render x = pretty 120 $ ppr x
      renderBody stms = putStrLn $ render $ Block (map BlockStm stms) noLoc
      showReinit prev spec = do
        putStrLn "-- ReinitAlloc"
        renderBody (reinitAllocStmts prev spec)
        putStrLn "-- ReinitInPlace"
        renderBody (reinitInPlaceStmts prev spec)

  putStrLn "====== a->b ======"
  showReinit (Just sa {fields = fa}) (sb {fields = fb})
  putStrLn "====== b->c ======"
  showReinit (Just sb {fields = fb}) (sc {fields = fc})
  putStrLn "====== c->d ======"
  showReinit (Just sc {fields = fc}) (sd {fields = fd})
  putStrLn "====== d->e ======"
  showReinit (Just sd {fields = fd}) (se {fields = fe})

test5 = pretty 100 (ppr $ fromJust $ toDecl $ head [cunit| void f() { return; } |]) == "void f();"
