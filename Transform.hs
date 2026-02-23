-- The transformation rewrites an original C AST into a hot-reloadable shape by
-- (1) collecting globals + static locals into a state record,
-- (2) rewriting identifier uses to field access (s->field),
-- (3) splitting main into Init/Update/Uninit,
-- (4) splicing the rewritten parts into a text-based template
{-# LANGUAGE ViewPatterns #-}
module Transform where

import Control.Applicative
import Control.Lens
import Control.Lens.Plated
import Data.Data.Lens (biplate)
import Data.Function (on)
import Data.List (find, findIndex, mapAccumL, partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, isNothing, catMaybes)
import Language.C hiding (mkIdent)
import Language.C.Data.Ident hiding (mkIdent)
import Control.Lens.Extras

data StateSpec = StateSpec
  { fields :: [StateField],
    initStmts :: [CStat],
    useRewrite :: [UseRewrite],
    hoistedNames :: [String]
  }

data StateField = StateField
  { fieldType :: CTypeSpec,
    fieldOrigName :: String,
    fieldName :: String,
    fieldInit :: Maybe CInit,
    fieldScope :: Maybe String
  }

data UseRewrite = UseRewrite
  { useName :: String,
    useScope :: Maybe String,
    useExpr :: CExpr
  }

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

-- Apply rewriting to the original AST: drop hoisted decls, split main, rewrite uses.
rewriteOrig :: StateSpec -> CTranslUnit -> CTranslUnit
rewriteOrig spec =
  dropHoistedDecls spec
    . splitMain spec
    . rewriteUses spec

--------------------------------------------------------------------------------
-- Collection helpers
--------------------------------------------------------------------------------

collectGlobalVars :: CTranslUnit -> [StateField]
collectGlobalVars (CTranslUnit decls _) = [ r
      | CDeclExt (CDecl specs declrs _) <- decls,
        not (isStaticSpec specs),
        declr <- declrs,
        Just r <- [fieldFromDecl specs Nothing declr] ]

-- ** `collectStaticLocals`
collectStaticLocals :: CTranslUnit -> [StateField]
collectStaticLocals tu = catMaybes [ fieldFromDecl specs (Just fn) declr
      | CFunDef _ (declrName -> Just fn) _ stmt _ <- tu ^.. template,
          CDecl specs declrs _ <- stmt ^.. template,
          isStaticSpec specs,
          declr <- declrs ]


--------------------------------------------------------------------------------
-- State derivation helpers
--------------------------------------------------------------------------------

-- ** `toStateFields`

-- - Merge globals + static locals into one field list.
-- - Ensure unique names (rename on collision with a numeric suffix).
toStateFields :: [StateField] -> [StateField]
toStateFields fields0 =
  snd $ mapAccumL rename Map.empty fields0
  where
    rename seen field =
      let base = fieldOrigName field
          idx = Map.findWithDefault 0 base seen
          newName = if idx == 0 then base else base <> show idx
          seen' = Map.insert base (idx + 1) seen
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

rewriteUses :: StateSpec -> CTranslUnit -> CTranslUnit
rewriteUses spec (CTranslUnit decls annot) =
  CTranslUnit (map rewriteExt decls) annot
  where
    rewrites = useRewrite spec

    rewriteExt (CFDefExt def@(CFunDef _ declr _ _ _)) =
      CFDefExt (def & template %~ rewriteExpr rewrites (declrName declr))
    rewriteExt ext =
      ext & template %~ rewriteExpr rewrites Nothing

lookupRewrite :: [UseRewrite] -> Maybe String -> String -> Maybe CExpr
lookupRewrite rewrites scope name =
  let scoped = find matchesScope rewrites
      global = find matchesGlobal rewrites
   in useExpr <$> (scoped <|> global)
  where
    matchesScope r = useName r == name && useScope r == scope
    matchesGlobal r = useName r == name && isNothing (useScope r)

rewriteExpr :: [UseRewrite] -> Maybe String -> CExpr -> CExpr
rewriteExpr rewrites scope expr =
  case expr of
    CVar ident _ -> fromMaybe expr (lookupRewrite rewrites scope (identName ident))
    _ -> expr

dropHoistedDecls :: StateSpec -> CTranslUnit -> CTranslUnit
dropHoistedDecls spec (CTranslUnit decls annot) =
  CTranslUnit (mapMaybe dropExt decls) annot
  where
    names = hoistedNames spec

    dropExt (CDeclExt decl) = CDeclExt <$> dropDecl False decl
    dropExt (CFDefExt def) = Just (CFDefExt (dropInFun def))
    dropExt ext = Just ext

    dropInFun (CFunDef specs declr decls stmt n) =
      CFunDef specs declr decls (dropInStmt stmt) n

    dropInStmt stmt =
      case stmt of
        CCompound labels items pos ->
          CCompound labels (mapMaybe dropItem items) pos
        CIf cond t e pos ->
          CIf cond (dropInStmt t) (fmap dropInStmt e) pos
        CWhile cond body isDo pos ->
          CWhile cond (dropInStmt body) isDo pos
        CFor initExpr cond step body pos ->
          CFor initExpr cond step (dropInStmt body) pos
        CSwitch expr body pos ->
          CSwitch expr (dropInStmt body) pos
        CLabel ident body attrs pos ->
          CLabel ident (dropInStmt body) attrs pos
        CCase expr body pos ->
          CCase expr (dropInStmt body) pos
        CCases l r body pos ->
          CCases l r (dropInStmt body) pos
        CDefault body pos ->
          CDefault (dropInStmt body) pos
        _ -> stmt

    dropItem (CBlockDecl decl) = CBlockDecl <$> dropDecl True decl
    dropItem (CBlockStmt s) = Just (CBlockStmt (dropInStmt s))
    dropItem item = Just item

    dropDecl requireStatic (CDecl specs declrs pos) =
      if requireStatic && not (isStaticSpec specs)
        then Just (CDecl specs declrs pos)
        else
          let declrs' = filter (not . shouldDrop) declrs
           in if null declrs'
                then Nothing
                else Just (CDecl specs declrs' pos)
    dropDecl _ d = Just d

    shouldDrop (Just declr, _, _) =
      maybe False (`elem` names) (declrName declr)
    shouldDrop _ = False

splitMain :: StateSpec -> CTranslUnit -> CTranslUnit
splitMain spec (CTranslUnit decls annot) =
  CTranslUnit (concatMap splitExt decls) annot
  where
    splitExt (CFDefExt def)
      | declrName (funDeclr def) == Just "main" =
          let (initDef, updateDef, uninitDef) = splitMainDef def
           in [CFDefExt initDef, CFDefExt updateDef, CFDefExt uninitDef]
    splitExt ext = [ext]

    splitMainDef (CFunDef _ _ _ stmt _) =
      case stmt of
        CCompound _ items pos ->
          let (preItems, loopItem, postItems) = splitOnLastWhile items
              (initItems, updatePrefix) = partition keepInitItem preItems
              (loopCond, loopBodyItems) = extractWhile loopItem
              initItems' = initItems <> map CBlockStmt (initStmts spec)
              updateItems =
                updatePrefix
                  <> loopBodyItems
                  <> maybe [] (\cond -> [CBlockStmt (CReturn (Just cond) undefNode)]) loopCond
              initBody = CCompound [] initItems' pos
              updateBody = CCompound [] updateItems pos
              uninitBody = CCompound [] postItems pos
           in ( mkFunDef "Init" (CVoidType undefNode) initBody,
                mkFunDef "Update" (CBoolType undefNode) updateBody,
                mkFunDef "Uninit" (CVoidType undefNode) uninitBody
              )
        _ ->
          ( mkFunDef "Init" (CVoidType undefNode) (CCompound [] [] undefNode),
            mkFunDef "Update" (CBoolType undefNode) (CCompound [] [] undefNode),
            mkFunDef "Uninit" (CVoidType undefNode) (CCompound [] [] undefNode)
          )

    splitOnLastWhile items =
      case findIndex isWhile (reverse items) of
        Nothing -> (items, Nothing, [])
        Just idxFromEnd ->
          let idx = length items - idxFromEnd - 1
              (pre, rest) = splitAt idx items
           in case rest of
                [] -> (items, Nothing, [])
                (loopItem : post) -> (pre, Just loopItem, post)

    extractWhile (Just (CBlockStmt (CWhile cond body _ _))) =
      (Just cond, compoundItems body)
    extractWhile _ = (Nothing, [])

    compoundItems (CCompound _ items _) = items
    compoundItems stmt = [CBlockStmt stmt]

    keepInitItem item =
      let names = map identName (toListOf (biplate :: Traversal' CBlockItem Ident) item)
       in all keepInit names

    funDeclr (CFunDef _ declr _ _ _) = declr

isWhile :: CBlockItem -> Bool
isWhile (CBlockStmt CWhile {}) = True
isWhile _ = False

--------------------------------------------------------------------------------
-- Template splicing helpers
--------------------------------------------------------------------------------

replaceStateStructFields :: [StateField] -> CTranslUnit -> CTranslUnit
replaceStateStructFields fs (CTranslUnit decls annot) =
  CTranslUnit (map replaceExt decls) annot
  where
    replaceExt (CDeclExt decl) = CDeclExt (replaceDecl decl)
    replaceExt ext = ext

    replaceDecl (CDecl specs declrs pos) =
      CDecl (map replaceSpec specs) declrs pos
    replaceDecl decl = decl

    replaceSpec (CTypeSpec (CSUType (CStruct tag (Just ident) (Just _) attrs pos1) pos2))
      | identName ident == "state" =
          let members = buildStateMembers fs
           in CTypeSpec (CSUType (CStruct tag (Just ident) (Just members) attrs pos1) pos2)
    replaceSpec spec = spec

replaceFuncBody :: String -> CStat -> CTranslUnit -> CTranslUnit
replaceFuncBody name newBody (CTranslUnit decls annot) =
  CTranslUnit (map replaceExt decls) annot
  where
    replaceExt (CFDefExt (CFunDef specs declr decls _ pos))
      | declrName declr == Just name =
          CFDefExt (CFunDef specs declr decls newBody pos)
    replaceExt ext = ext

extractFuncBody :: String -> CTranslUnit -> CStat
extractFuncBody name (CTranslUnit decls _) =
  fromMaybe (CCompound [] [] undefNode) (findBody decls)
  where
    findBody [] = Nothing
    findBody (CFDefExt (CFunDef _ declr _ body _) : rest)
      | declrName declr == Just name = Just body
    findBody (_ : rest) = findBody rest

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

declrHasFun :: CDeclr -> Bool
declrHasFun (CDeclr _ derived _ _ _) =
  any isFun derived
  where
    isFun CFunDeclr {} = True
    isFun _ = False

declrName :: CDeclr -> Maybe String
declrName (CDeclr (Just ident) _ _ _ _) = Just (identName ident)
declrName _ = Nothing

declTypeSpec :: [CDeclSpec] -> Maybe CTypeSpec
declTypeSpec specs =
  case [ts | CTypeSpec ts <- specs] of
    (ts : _) -> Just ts
    [] -> Nothing

fieldFromDecl :: [CDeclSpec] -> Maybe String -> (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> Maybe StateField
fieldFromDecl specs scope (Just declr, initVal, _)
  | not (declrHasFun declr) =
      case (declTypeSpec specs, declrName declr) of
        (Just ty, Just name) ->
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
buildStateMembers fs =
  map buildDecl grouped
  where
    grouped = groupByType fs

    buildDecl fields' =
      let ty = fieldType (head fields')
          declrs = map buildDeclr fields'
       in CDecl [CTypeSpec ty] declrs undefNode

    buildDeclr field =
      (Just (CDeclr (Just (mkIdent (fieldName field))) [] Nothing [] undefNode), Nothing, Nothing)

    groupByType [] = []
    groupByType (x : xs) =
      let (same, rest) = span ((== show (fieldType x)) . show . fieldType) xs
       in (x : same) : groupByType rest

mkFunDef :: String -> CTypeSpec -> CStat -> CFunDef
mkFunDef name retType body =
  CFunDef
    [CTypeSpec retType]
    (mkFunDeclr name)
    []
    body
    undefNode

mkFunDeclr :: String -> CDeclr
mkFunDeclr name =
  CDeclr
    (Just (mkIdent name))
    [CFunDeclr (Right ([mkStateParam "s"], False)) [] undefNode]
    Nothing
    []
    undefNode

mkStateParam :: String -> CDecl
mkStateParam name =
  CDecl
    [CTypeSpec (CSUType (CStruct CStructTag (Just (mkIdent "state")) Nothing [] undefNode) undefNode)]
    [ (Just (CDeclr (Just (mkIdent name)) [CPtrDeclr [] undefNode] Nothing [] undefNode), Nothing, Nothing)
    ]
    undefNode
