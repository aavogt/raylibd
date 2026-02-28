module Transform (substituteTemplate, Prev (..), buildStateSpec) where

import Control.Lens hiding (Const)
import Control.Lens.Extras
import Data.Data
import Data.List
import Data.Loc (noLoc)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
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
    useRewrite :: [UseRewrite],
    hoistedNames :: [String]
  }
  deriving (Show)

data StateField = StateField
  { fieldType :: TypeSpec,
    fieldOrigName :: String,
    fieldName :: String,
    fieldInit :: Maybe Initializer,
    fieldScope :: Maybe String
  }
  deriving (Show)

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
      fields = toStateFields (globals <> statics)
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
toInitStmts =
  mapMaybe toInit
  where
    toInit field =
      (\fInit -> Exp (Just (mkAssign field fInit)) noLoc) <$> fieldInit field
    mkAssign field initVal =
      Assign
        (mkMember (fieldName field))
        JustAssign
        (initToExpr (fieldType field) initVal)
        noLoc

toUseRewrite :: [StateField] -> [UseRewrite]
toUseRewrite =
  map toRewrite
  where
    toRewrite field =
      UseRewrite
        { useName = fieldOrigName field,
          useScope = fieldScope field,
          useExpr = mkMember (fieldName field)
        }

instance Plated Exp

applyRewrites :: (Data a) => [UseRewrite] -> a -> a
applyRewrites rewrites =
  template %~ rewrite \case
    Var (Id a _) _ | Just (UseRewrite {useExpr}) <- find ((a ==) . useName) rewrites -> Just useExpr
    _ -> Nothing

newtype Prev = Prev {prevSpec :: Maybe StateSpec}

substituteTemplate from spec Prev {..} =
  let render x = pretty 120 $ ppr x
      Just (Bodies {..}) = getBodies spec prevSpec from
      renderDecls = concatMap ((++ ";") . render)
      mergedSF = maybe id (mergeSF . fields) prevSpec (fields spec)
      withTemplate =
        lined %~ \case
          "//STRUCTBODY" -> renderDecls (buildStateMembers mergedSF)
          "//REINITBODY" -> render reinitBody
          "//INITBODY" -> render initBody
          "//STEPBODY" -> render stepBody
          "//UNINITBODY" -> render uninitBody
          x -> x
   in (Prev {prevSpec = Just spec}, withTemplate)

-- FIXME
-- - reuse dummies of the same type, instead of ++ notfound, it becomes a fold over notFound attempting to insert
-- - sortBy is probably better than this, possibly make sure old is always sorted
-- - produce reinitBody here?
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
      | otherwise = StateField {fieldName = "dummy" ++ show i, fieldInit = Nothing, fieldScope = Nothing, ..}

data Bodies = Bodies {initBody, stepBody, reinitBody, uninitBody :: Stm}
  deriving (Data)

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
       in Just (Bodies {..}) & applyRewrites (useRewrite spec)

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
  toInitStmts
    [ field
      | field <- fields spec,
        fromMaybe False $ liftA2 cinitNE (fieldInit field) (lookupPrev field)
    ]
  where
    prevMap =
      M.fromList
        [ ((fieldOrigName f, fieldScope f), initVal)
          | f <- fields prevSpec,
            Just initVal <- [fieldInit f]
        ]
    lookupPrev f = M.lookup (fieldOrigName f, fieldScope f) prevMap

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
  let fn = [cunit| void f() { while (true) { static int x = 0; } } |]
  pPrint fn
  pPrint $ collectStaticLocals fn

isStaticSpec :: DeclSpec -> Bool
isStaticSpec (DeclSpec storage _ _ _) =
  isJust $
    find
      ( \case
          Tstatic {} -> True
          _ -> False
      )
      storage
isStaticSpec _ = False

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

fieldFromDecl :: DeclSpec -> Maybe String -> Init -> Maybe StateField
fieldFromDecl specs scope (Init ident decl _ initVal _ _)
  | declrHasntFun decl =
      let ty = declTypeSpec specs
          name = identName ident
       in Just
            StateField
              { fieldType = ty,
                fieldOrigName = name,
                fieldName = name,
                fieldInit = initVal,
                fieldScope = scope
              }
fieldFromDecl _ _ _ = Nothing

identName :: Id -> String
identName (Id name _) = name

mkIdent :: String -> Id
mkIdent name = Id name noLoc

mkMember :: String -> Exp
mkMember name = [cexp| s->$id:name |]

initToExpr :: TypeSpec -> Initializer -> Exp
initToExpr _ (ExpInitializer expr _) = expr
initToExpr ty (CompoundInitializer items _) =
  CompoundLit (Type (DeclSpec [] [] ty noLoc) (DeclRoot noLoc) noLoc) items noLoc
initToExpr _ _ = Const (IntConst "0" Signed 0 noLoc) noLoc

buildStateMembers :: [StateField] -> [InitGroup]
buildStateMembers = map buildDecl
  where
    buildDecl StateField {..} =
      let declSpec = DeclSpec [] [] fieldType noLoc
          initDecl = Init (mkIdent fieldName) (DeclRoot noLoc) Nothing Nothing [] noLoc
       in InitGroup declSpec [] [initDecl] noLoc
