{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Transform where

import Control.Lens hiding (Const)
import Control.Lens.Extras
import Control.Monad.Trans.State
import Data.Data
import Data.List
import Data.Loc (noLoc)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace
import Language.C hiding (mkIdent)
import Language.C.Quote.C
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class (ppr)
import Text.Show.Pretty (pPrint, ppShow)

pattern Fun id block <-
  ( \case
      Func _ id _ _ block _ -> (identName id, block)
      OldFunc _ id _ _ _ block _ -> (identName id, block) ->
      (id, block)
    )

data StateSpec = StateSpec
  { fields :: [StateField],
    initStmts :: [Stm],
    useRewrite :: Id -> [UseRewrite],
    hoistedNames :: [String]
  }

data StateField = StateField
  { fieldType :: TypeSpec,
    fieldOrigName :: String,
    fieldName :: String,
    fieldInit :: Maybe Initializer,
    fieldScope :: Maybe String,
    fieldArraySize :: [Const]
  }
  deriving Show

data UseRewrite = UseRewrite
  { useName :: String,
    useScope :: Maybe String,
    useExpr :: Exp
  }
  deriving (Show)

-- Collect globals + static locals and build derived rewrite info.
buildStateSpec :: [Definition] -> StateSpec
buildStateSpec ast =
  let globals = collectGlobalVars ast
      statics = collectStaticLocals ast
      fields = uniqueDummy $ toStateFields (globals <> statics)
      initStmts = toInitStmts fields
      useRewrite = toUseRewrite fields
      hoistedNames = map fieldOrigName fields
   in StateSpec {..}

--------------------------------------------------------------------------------
-- Collection helpers
--------------------------------------------------------------------------------

collectGlobalVars :: [Definition] -> [StateField]
collectGlobalVars decls =
  [ field
    | DecDef (InitGroup specs _ inits _) _ <- decls,
      not (isStaticSpec specs),
      initDecl <- inits,
      Just field <- [fieldFromDecl specs Nothing initDecl]
  ]

-- ** `collectStaticLocals`

collectStaticLocals :: [Definition] -> [StateField]
collectStaticLocals tu =
  catMaybes
    [ fieldFromDecl specs (Just fn) initDecl
      | FuncDef (Fun fn items) _ <- tu ^.. template,
        InitGroup specs _ inits _ <- items ^.. template,
        isStaticSpec specs,
        initDecl <- inits
    ]

--------------------------------------------------------------------------------
-- State derivation helpers
--------------------------------------------------------------------------------

-- ** `toStateFields`

-- - Merge globals + static locals into one field list.
-- - Ensure unique names (rename on collision with a numeric suffix).
toStateFields :: [StateField] -> [StateField]
toStateFields fields0 =
  snd $ mapAccumL rename M.empty fields0
  where
    rename seen field =
      let base = fieldOrigName field
          idx = M.findWithDefault 0 base seen
          newName = if idx == 0 then base else base <> show idx
          seen' = M.insert base (idx + 1) seen
       in (seen', field {fieldName = newName})

toInitStmts :: [StateField] -> [Stm]
toInitStmts sfs =
  [ case mlengths of
      Just lengths -> genLoops lengths \vs ->
        let lhs = indexExpr [cexp| s->$id:fieldName |] vs
         in [cstm| $lhs = $initExpr; |]
      _ -> [cstm| s->$id:fieldName = $initExpr; |]
    | field@StateField {fieldInit = Just initVal, fieldArraySize = mapM constToArrayLen -> mlengths, ..} <- sfs,
      let initExpr = initToExpr fieldType initVal
  ]

toUseRewrite :: [StateField] -> Id -> [UseRewrite]
toUseRewrite fs s =
  let ?s = s
   in map toRewrite fs
  where
    toRewrite field =
      UseRewrite
        { useName = fieldOrigName field,
          useScope = fieldScope field,
          useExpr = mkMember (fieldName field)
        }

-- StateField Maybe
uniqueDummy :: [StateField] -> [StateField]
uniqueDummy sfs =
  sequence
    [ do
        n <- case fieldName sf of
          "" -> popDummy
          name -> return name
        return sf {fieldName = n}
      | sf <- sfs
    ]
    `evalState` dummyNames
  where
    userNames = S.fromList [n | StateField {fieldName = n} <- sfs, n /= ""]
    dummyNames = filter (`S.notMember` userNames) $ map (\i -> "dummy" ++ show i) [0 ..]
    popDummy = do
      ~(x : xs) <- get
      put xs
      return x

instance Plated Exp

applyRewrites :: (Data a) => [UseRewrite] -> a -> a
applyRewrites rewrites =
  template %~ rewrite \case
    Var (Id a _) _ | Just (UseRewrite {useExpr}) <- find ((a ==) . useName) rewrites -> Just useExpr
    _ -> Nothing

data Prev = Prev {prevSpec :: Maybe StateSpec, prevSF :: [StateField]}

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
          "//REINITBODY" -> render reinitBody
          "//INITBODY" -> render initBody
          "//STEPBODY" -> render stepBody
          "//UNINITBODY" -> render uninitBody
          x -> x
   in (Prev {prevSpec = Just spec, prevSF = mergedSF}, withTemplate)

toDecl :: Definition -> Maybe Definition
-- this clause is wrong funcdef becomes initgroup:
-- DecDef
--     (InitGroup
--        (DeclSpec [] [] (Tvoid noLoc) noLoc)
--        []
--        [ Init
--            (Id "f" noLoc)
--            (Proto
--               (DeclRoot noLoc)
--               (Params
--                  [ Param
--                      Nothing (DeclSpec [] [] (Tvoid noLoc) noLoc) (DeclRoot noLoc) noLoc
--                  ]
--                  False
--                  noLoc)
--               noLoc)
--            Nothing
--            Nothing
--            []
--            noLoc
--        ]
--        noLoc)
--     noLoc
-- , FuncDef
--     (Func
--        (DeclSpec [] [] (Tvoid noLoc) noLoc)
--        (Id "f" noLoc)
--        (DeclRoot noLoc)
--        (Params
--           [ Param
--               Nothing (DeclSpec [] [] (Tvoid noLoc) noLoc) (DeclRoot noLoc) noLoc
--           ]
--           False
--           noLoc)
--        [ BlockStm
--            (Exp
--               (Just (FnCall (Var (Id "printf" noLoc) noLoc) [] noLoc)) noLoc)
--        ]
--        noLoc)
--     noLoc
toDecl (FuncDef f s) | f ^. funcName /= "main" = Just $ DecDef (InitGroup (f ^. funcDs) [] [Init (f ^. funcId) proto Nothing Nothing [] noLoc] noLoc) noLoc
  where
    proto = Proto (DeclRoot noLoc) (f ^. funcParams) noLoc
--
-- InitGroup    DeclSpec [Attr] [Init]    !SrcLoc
-- data Func  =  Func    DeclSpec Id Decl Params                   [BlockItem] !SrcLoc
--            |  OldFunc DeclSpec Id Decl [Id] (Maybe [InitGroup]) [BlockItem] !SrcLoc
toDecl td@(DecDef (TypedefGroup {}) _) = Just td
toDecl _ = Nothing

funcParams :: Lens' Func Params
funcParams op (Func a b c d e f) = op d <&> \b' -> Func a b c d e f

-- funcParams op (OldFunc a b c d e f g) = op (_ d e) <&> \b' -> OldFunc a b c d e f g

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

-- TODO partition into (declaration, definition)
dropMainNonStatic :: StateSpec -> [Definition] -> [Definition]
dropMainNonStatic spec = applyRewrites (useRewrite spec (mkIdent "s_top")) . filter (\d -> all ($ d) [notMain, notInit, notTypedef])
  where
    notTypedef (DecDef TypedefGroup {} _) = False
    notTypedef _ = True

    notMain (FuncDef (Fun "main" _) _) = False
    notMain _ = True

    notInit (DecDef (InitGroup specs _ inits _) _) = isStaticSpec specs
    notInit _ = True

-- FIXME
-- - reuse dummies of the same type, instead of ++ notfound, it becomes a fold over notFound attempting to insert
-- - sortBy is probably better than this, possibly make sure old is always sorted
-- - produce reinitBody here?
--
-- This ends up producing:
-- struct state {
-- Seg dummy0[2];
-- int nframe;
-- Seg dummy2[3];
-- Seg dummy3[4];
-- Seg dummy4[5];
-- Seg dummy5[6];
-- Seg dummy6[7];
-- Seg segs[2];
--
-- };
-- struct prevstate {
-- Seg dummy0[2];
-- int nframe;
-- Seg dummy2[3];
-- Seg dummy3[4];
-- Seg dummy4[5];
-- Seg dummy5[6];
-- Seg segs[7];
--
-- };
-- 1. merge dummies? But dummyi should be a different constructor?
mergeSF :: [StateField] -> [StateField] -> [StateField]
mergeSF old new = expandedDummies ++ map snd notFound
  where
    oldDeclSet = S.fromList oldDecl
    oldDecl = map show $ buildStateMembers old
    newDecl = map show $ buildStateMembers new
    (sameDecl, notFound) = zip newDecl new & partition ((`S.member` oldDeclSet) . fst)
    sameDeclSet = S.fromList $ map fst sameDecl
    expandedDummies = zipWith3 dummyWhenMissing [0 ..] old oldDecl
    dummyWhenMissing i o@StateField {..} oStr
      | oStr `S.member` sameDeclSet = o
      | otherwise = StateField {fieldName = "", fieldInit = Nothing, fieldScope = Nothing, ..}

data Bodies = Bodies {initBody, stepBody, reinitBody, uninitBody :: Stm}
  deriving (Data, Show)

getBodies :: StateSpec -> Maybe StateSpec -> [Definition] -> Maybe Bodies
getBodies spec prevSpec decls = listToMaybe $ mapMaybe splitMainDef decls
  where
    splitMainDef (FuncDef (Fun "main" items) _) = withItems items
    splitMainDef _ = Nothing

    withItems items =
      let (preItems, loopItem, postItems) = splitOnLastWhile items
          (initItems, updatePrefix) = partition keepInitItem preItems
          (loopCond, loopBodyItems) = extractWhile loopItem
          initItems' = initItems <> map BlockStm (initStmts spec)
          stepItems =
            updatePrefix
              <> loopBodyItems
              <> maybe [] (\cond -> [BlockStm (Return (Just cond) noLoc)]) loopCond
          initBody = Block initItems' noLoc
          stepBody = Block stepItems noLoc
          uninitBody = Block postItems noLoc
          reinitBody = Block (map BlockStm (reinitStmts prevSpec spec)) noLoc
          structBody = buildStateMembers (fields spec)
       in Just (Bodies {..}) & applyRewrites (useRewrite spec (mkIdent "s"))

    splitOnLastWhile items =
      break isWhile (reverse items)
        & \case
          (a, b : cs) -> (reverse cs, Just b, reverse a)
          (a, cs) -> (reverse cs, Nothing, reverse a)

    extractWhile (Just (BlockStm (While cond body _))) =
      (Just cond, compoundItems body)
    extractWhile _ = (Nothing, [])

    compoundItems (Block items _) = items
    compoundItems stmt = [BlockStm stmt]

    keepInitItem item =
      let names = map identName (toListOf biplate item)
       in all keepInit names

-- | reinitStmts s t is the body of Reinit(state *s, state *t)
-- s previous, t new
reinitStmts :: Maybe StateSpec -> StateSpec -> [Stm]
reinitStmts Nothing _ = []
reinitStmts (Just prevSpec) spec =
  [ [cstm| if (t) { $stms:reinitToNew } else { $stms:reinitInPlace } |]
  ]
  where
    prevMap =
      M.fromList
        [ ((fieldOrigName f, fieldScope f), f)
          | f <- fields prevSpec
        ]

    lookupPrevField f = M.lookup (fieldOrigName f, fieldScope f) prevMap

    reinitInPlace = concatMap (reinitTarget "s") (fields spec)

    reinitTarget target field =
      case lookupPrevField field of
        Nothing -> initTarget target field (fieldInit field)
        Just prevField
          | initEqual (fieldInit field) (fieldInit prevField) -> []
          | otherwise -> initTarget target field (fieldInit field)

    reinitToNew = concatMap reinitFieldToNew (fields spec)

    reinitFieldToNew field =
      case lookupPrevField field of
        Just prevField
          | initEqual (fieldInit field) (fieldInit prevField) -> copyField prevField field
        _ -> initTarget "t" field (fieldInit field)

initTarget _ _ Nothing = []
initTarget target StateField {..} (Just initVal) =
  case mapM constToArrayLen fieldArraySize of
    Just indexBounds -> [genLoopsST fieldName indexBounds]
    Nothing -> [[cstm| $id:target->$id:fieldName = $(initToExpr fieldType initVal); |]]

copyField prevField field =
  case ( mapM constToArrayLen $ fieldArraySize field,
         mapM constToArrayLen $ fieldArraySize prevField
       ) of
    (Just [newLen], Just [prevLen]) ->
      [ [cstm| t->$id:(fieldName field)[$int:idx] = s->$id:(fieldName field)[$int:idx]; |]
        | idx <- [0 .. min newLen prevLen - 1]
      ]
    (Nothing, Nothing) -> [[cstm| t->$id:(fieldName field) = s->$id:(fieldName prevField); |]]
    _ -> initTarget "t" field (fieldInit field)

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

isWhile :: BlockItem -> Bool
isWhile (BlockStm (While {})) = True
isWhile _ = False

--------------------------------------------------------------------------------
-- Helper utilities
--------------------------------------------------------------------------------

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

genLoopsST :: String -> [Int] -> Stm
genLoopsST fieldName bounds = genLoops bounds \vs ->
  let lhs = indexExpr [cexp| t->$id:fieldName |] vs
      rhs = indexExpr [cexp| s->$id:fieldName  |] vs
   in [cstm| $exp:lhs = $exp:rhs; |]

genLoops :: [Int] -> ([String] -> Stm) -> Stm
genLoops bounds mkBody =
  let -- indexes = [ i,j,k ... i1, j1 ... ]
      indexes = [pre : suffix | suffix <- "" : map show [1 ..], pre <- ['i' .. 'z']]
      vs = take (length bounds) indexes
      step body (v, n) = [cstm| for (int $id:v = 0; $id:v < $int:n; $id:v++) $stm:body |]
   in foldl step (mkBody vs) (zip vs bounds)

-- | indexExpr arr [i,j,k] is arr[i][j][k]
indexExpr :: Exp -> [String] -> Exp
indexExpr = foldl \e v -> [cexp| $exp:e[$id:v] |]

isStaticSpec :: DeclSpec -> Bool
isStaticSpec = anyOf template \case Tstatic {} -> True; _ -> False

declrHasntFun :: Decl -> Bool
declrHasntFun (DeclRoot _) = True
declrHasntFun (Ptr _ decl _) = declrHasntFun decl
declrHasntFun (Array _ _ decl _) = declrHasntFun decl
declrHasntFun (Proto {}) = False
declrHasntFun (OldProto {}) = False
declrHasntFun (BlockPtr _ decl _) = declrHasntFun decl
declrHasntFun (AntiTypeDecl _ _) = True

declTypeSpec :: DeclSpec -> TypeSpec
declTypeSpec (DeclSpec _ _ ty _) = ty
declTypeSpec _ = Tvoid noLoc

declArraySize :: Decl -> [Const]
declArraySize (Ptr _ decl _) = declArraySize decl
declArraySize (Array _ (ArraySize _ (Const sizeConst _) _) d _) = sizeConst : declArraySize d
declArraySize (Array _ _ decl _) = declArraySize decl
declArraySize (BlockPtr _ decl _) = declArraySize decl
declArraySize _ = []

fieldFromDecl :: DeclSpec -> Maybe String -> Init -> Maybe StateField
fieldFromDecl specs scope (Init ident decl _ initVal _ _)
  | declrHasntFun decl =
      let ty = declTypeSpec specs
          name = identName ident
          arraySize = declArraySize decl
       in Just
            StateField
              { fieldType = ty,
                fieldOrigName = name,
                fieldName = name,
                fieldInit = initVal,
                fieldScope = scope,
                fieldArraySize = arraySize
              }
fieldFromDecl _ _ _ = Nothing

identName :: Id -> String
identName (Id name _) = name

mkMember name = let sname = ?s in [cexp| $id:sname->$id:name |]

mkMemberFrom :: String -> String -> Exp
mkMemberFrom target name = [cexp| $id:target->$id:name |]

mkMemberIndexFrom :: String -> String -> Int -> Exp
mkMemberIndexFrom target name idx =
  Index
    [cexp| $id:target->$id:name |]
    (Const (IntConst (show idx) Signed (fromIntegral idx) noLoc) noLoc)
    noLoc

initToExpr :: TypeSpec -> Initializer -> Exp
initToExpr _ (ExpInitializer expr _) = expr
initToExpr ty (CompoundInitializer items _) =
  CompoundLit (Type (DeclSpec [] [] ty noLoc) (DeclRoot noLoc) noLoc) items noLoc
initToExpr _ _ = Const (IntConst "0" Signed 0 noLoc) noLoc

initExprAt :: Int -> Initializer -> Exp
initExprAt _ (ExpInitializer expr _) = expr
initExprAt idx (CompoundInitializer items _) =
  case positionalInitAt idx items of
    Just initVal -> initExprAt idx initVal
    Nothing -> Const (IntConst "0" Signed 0 noLoc) noLoc
initExprAt _ _ = Const (IntConst "0" Signed 0 noLoc) noLoc

positionalInitAt :: Int -> [(Maybe Designation, Initializer)] -> Maybe Initializer
positionalInitAt idx items =
  let positional = [initVal | (Nothing, initVal) <- items]
   in if idx >= 0 && idx < length positional
        then Just (positional !! idx)
        else Nothing

constToArrayLen :: Const -> Maybe Int
constToArrayLen (IntConst lit _ _ _) =
  case reads lit of
    [(n, _)] | n >= (0 :: Integer) -> Just (fromInteger n)
    _ -> Nothing
constToArrayLen _ = Nothing

buildStateMembers :: [StateField] -> [InitGroup]
buildStateMembers = map buildDecl
  where
    buildDecl StateField {..} =
      let declSpec = DeclSpec [] [] fieldType noLoc
          declRoot = foldl (\y x -> Array [] (ArraySize False (Const x noLoc) noLoc) y noLoc) (DeclRoot noLoc) fieldArraySize
          initDecl = Init (mkIdent fieldName) declRoot Nothing Nothing [] noLoc
       in InitGroup declSpec [] [initDecl] noLoc

mkIdent :: String -> Id
mkIdent name = Id name noLoc
