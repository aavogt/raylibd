module Transform (substituteTemplate, Prev(..), buildStateSpec) where

import Control.Lens
import Control.Lens.Extras
import Data.Data
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Language.C hiding (mkIdent)
import Language.C.Data.Ident hiding (mkIdent)
import qualified Data.Set as S
import Debug.Trace

data StateSpec = StateSpec
  { fields :: [StateField],
    initStmts :: [CStat],
    useRewrite :: [UseRewrite],
    hoistedNames :: [String]
  }
  deriving (Show)

data StateField = StateField
  { fieldType :: CTypeSpec,
    fieldOrigName :: String,
    fieldName :: String,
    fieldInit :: Maybe CInit,
    fieldScope :: Maybe String
  }
  deriving (Show)

data UseRewrite = UseRewrite
  { useName :: String,
    useScope :: Maybe String,
    useExpr :: CExpr
  }
  deriving (Show)

-- Collect globals + static locals and build derived rewrite info.
buildStateSpec :: CTranslUnit -> StateSpec
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

collectGlobalVars :: CTranslUnit -> [StateField]
collectGlobalVars (CTranslUnit decls _) =
  [ r
    | CDeclExt (CDecl specs declrs _) <- decls,
      not (isStaticSpec specs),
      declr <- declrs,
      Just r <- [fieldFromDecl specs Nothing declr]
  ]

-- ** `collectStaticLocals`

collectStaticLocals :: CTranslUnit -> [StateField]
collectStaticLocals tu =
  catMaybes
    [ fieldFromDecl specs (Just fn) declr
      | CFunDef _ (declrName -> Just fn) _ stmt _ <- tu ^.. template,
        CDecl specs declrs _ <- stmt ^.. template,
        isStaticSpec specs,
        declr <- declrs
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

toInitStmts :: [StateField] -> [CStat]
toInitStmts =
  mapMaybe toInit
  where
    toInit field =
      (\fInit -> CExpr (Just (mkAssign field fInit)) undefNode) <$> fieldInit field
    mkAssign field initVal =
      CAssign
        CAssignOp
        (mkMember (fieldName field))
        (initToExpr (fieldType field) initVal)
        undefNode

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

instance Plated CExpr

applyRewrites :: (Data a) => [UseRewrite] -> a -> a
applyRewrites rewrites =
  template %~ rewrite \case
    CVar (Ident a b c) _ | Just (UseRewrite {useExpr}) <- find ((a ==) . useName) rewrites -> Just useExpr
    _ -> Nothing

newtype Prev = Prev { prevSpec :: Maybe StateSpec }

substituteTemplate from spec Prev{.. } =
  let render x = show $ Language.C.pretty x
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
  in (Prev{ prevSpec = Just spec }, withTemplate)

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
  expandedDummies = zipWith3 dummyWhenMissing [0 .. ] old oldDecl
  dummyWhenMissing i o@StateField{..} oStr
      | oStr `S.member` sameDeclSet = o
      | otherwise = StateField { fieldName = "dummy"++show i, fieldInit = Nothing, fieldScope = Nothing, ..}

data Bodies = Bodies {initBody, stepBody, reinitBody, uninitBody :: CStat}
  deriving (Data)

getBodies :: StateSpec -> Maybe StateSpec -> CTranslUnit -> Maybe Bodies
getBodies spec prevSpec (CTranslUnit decls annot) = listToMaybe $ mapMaybe splitExt decls
  where
    splitExt (CFDefExt def)
      | declrName (funDeclr def) == Just "main" = splitMainDef def
    splitExt ext = Nothing

    splitMainDef (CFunDef _ _ _ stmt _) =
      case stmt of
        CCompound _ items pos ->
          let (preItems, loopItem, postItems) = splitOnLastWhile items
              (initItems, updatePrefix) = partition keepInitItem preItems
              (loopCond, loopBodyItems) = extractWhile loopItem
              initItems' = initItems <> map CBlockStmt (initStmts spec)
              stepItems =
                updatePrefix
                  <> loopBodyItems
                  <> maybe [] (\cond -> [CBlockStmt (CReturn (Just cond) undefNode)]) loopCond
              initBody = CCompound [] initItems' pos
              stepBody = CCompound [] stepItems pos
              uninitBody = CCompound [] postItems pos
              reinitBody = CCompound [] (map CBlockStmt (reinitStmts prevSpec spec)) pos
              structBody = buildStateMembers (fields spec)
           in Just (Bodies {..}) & applyRewrites (useRewrite spec)
        _ -> Nothing

    splitOnLastWhile items = break isWhile (reverse items)
      & \case
          (a, b:cs) -> (reverse cs, Just b, reverse a)
          (a, cs) -> (reverse cs, Nothing, reverse a)

    extractWhile (Just (CBlockStmt (CWhile cond body _ _))) =
      (Just cond, compoundItems body)
    extractWhile _ = (Nothing, [])

    compoundItems (CCompound _ items _) = items
    compoundItems stmt = [CBlockStmt stmt]

    keepInitItem item =
      let names = map identName (toListOf (biplate :: Traversal' CBlockItem Ident) item)
       in all keepInit names

    funDeclr (CFunDef _ declr _ _ _) = declr

-- | reinitStmts s t is the body of Reinit(state *s, state *t)
-- s previous, t new
--
reinitStmts ::  Maybe StateSpec -> StateSpec -> [CStat]
reinitStmts Nothing _ = []
reinitStmts (Just prevSpec) spec = traceShow (prevSpec, spec) $
  toInitStmts [ field |
      field <- fields spec,
      fromMaybe True $ liftA2 cinitNE (fieldInit field) (lookupPrev field) ]
  where
    prevMap =
      M.fromList
        [ ((fieldOrigName f, fieldScope f), initVal)
          | f <- fields prevSpec,
            Just initVal <- [fieldInit f]
        ]
    lookupPrev f = M.lookup (fieldOrigName f, fieldScope f) prevMap

cinitNE :: CInit -> CInit -> Bool
cinitNE (CInitExpr (CConst a) _) (CInitExpr (CConst b) _) = not (cinitEQ a b)
cinitNE _ _ = False

cinitEQ :: CConst -> CConst -> Bool
cinitEQ (CStrConst s _) (CStrConst t _) = s == t
cinitEQ (CIntConst s _) (CIntConst t _) = s == t
cinitEQ (CFloatConst s _) (CFloatConst t _) = s == t
cinitEQ _ _ = False

isWhile :: CBlockItem -> Bool
isWhile (CBlockStmt CWhile {}) = True
isWhile _ = False

--------------------------------------------------------------------------------
-- Helper utilities
--------------------------------------------------------------------------------

keepInit :: String -> Bool
keepInit _ = True

isStaticSpec :: [CDeclSpec] -> Bool
isStaticSpec = any isStatic
  where
    isStatic (CStorageSpec (CStatic _)) = True
    isStatic _ = False

declrHasntFun :: CDeclr -> Bool
declrHasntFun (CDeclr _ derived _ _ _) = null [ () | CFunDeclr{} <- derived ]

declrName :: CDeclr -> Maybe String
declrName (CDeclr (Just ident) _ _ _ _) = Just (identName ident)
declrName _ = Nothing

declTypeSpec :: [CDeclSpec] -> [CTypeSpec]
declTypeSpec specs = [ts | CTypeSpec ts <- specs]

fieldFromDecl :: [CDeclSpec] -> Maybe String -> (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> Maybe StateField
fieldFromDecl specs scope (Just declr, initVal, _)
  | declrHasntFun declr =
      case (declTypeSpec specs, declrName declr) of
        (ty : _, Just name) ->
          Just
            StateField
              { fieldType = ty,
                fieldOrigName = name,
                fieldName = name,
                fieldInit = initVal,
                fieldScope = scope
              }
        _ -> Nothing
fieldFromDecl _ _ _ = Nothing

identName :: Ident -> String
identName (Ident name _ _) = name

mkIdent :: String -> Ident
mkIdent name = Ident name 0 undefNode

mkVar :: String -> CExpr
mkVar name = CVar (mkIdent name) undefNode

mkMember :: String -> CExpr
mkMember name = CMember (mkVar "s") (mkIdent name) True undefNode

initToExpr :: CTypeSpec -> CInit -> CExpr
initToExpr _ (CInitExpr expr _) = expr
initToExpr ty (CInitList items _) =
  CCompoundLit (CDecl [CTypeSpec ty] [(Nothing, Nothing, Nothing)] undefNode) items undefNode

buildStateMembers :: [StateField] -> [CDecl]
buildStateMembers = map buildDecl
  where
    buildDecl StateField { .. } =
      let declr = (Just (CDeclr (Just (mkIdent fieldName)) [] Nothing [] undefNode), Nothing, Nothing)
      in CDecl [CTypeSpec fieldType] [declr] undefNode
