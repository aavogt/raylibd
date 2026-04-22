module Rename
  ( rename,
  )
where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Language.C.Quote
import Control.Lens
import Data.Data.Lens (biplate)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.Trans.Class
import Data.Maybe


-- | Rename locally scoped variables that shadow names from outer scopes.
rename :: [Definition] -> [Definition]
rename defs =
  let env = Env {envUsedNames = collectUsedNames defs, envGlobalNames = collectGlobalNames defs}
   in evalState (runReaderT (mapM renameDefinition defs) env) 0

collectUsedNames :: [Definition] -> UsedNames
collectUsedNames defs = S.fromList [name | Id name _ <- defs ^.. biplate]

collectGlobalNames :: [Definition] -> S.Set String
collectGlobalNames defs = S.fromList (concatMap collectGlobal defs)
  where
    collectGlobal def = case def of
      FuncDef (Func _ ident _ _ _ _) _ -> [identName ident]
      FuncDef (OldFunc _ ident _ _ _ _ _) _ -> [identName ident]
      DecDef (InitGroup _ _ inits _) _ -> map initName inits
      _ -> []

    initName (Init ident _ _ _ _ _) = identName ident

    identName (Id name _) = name
    identName _ = ""

--------------------------------------------------------------------------------
-- Environment
--------------------------------------------------------------------------------

type UsedNames = S.Set String

data Env = Env
  { envUsedNames :: UsedNames,
    envGlobalNames :: S.Set String
  }

type RenameM = ReaderT Env (State Int)

data Scope = Scope
  { scopeBindings :: M.Map String String
  , scopeUsed :: S.Set String
  }

type ScopeStack = [Scope]

type LocalM = StateT ScopeStack RenameM

emptyScope :: Scope
emptyScope = Scope {scopeBindings = M.empty, scopeUsed = S.empty}

pushScope :: LocalM ()
pushScope = modify (emptyScope :)

popScope :: LocalM ()
popScope = modify \case
  [] -> []
  (_ : rest) -> rest

registerUsed :: String -> LocalM ()
registerUsed name = modify \case
  [] -> [emptyScope {scopeUsed = S.singleton name}]
  (s : rest) -> s {scopeUsed = S.insert name (scopeUsed s)} : rest

withScope :: LocalM a -> LocalM a
withScope action = do
  pushScope
  result <- action
  popScope
  return result

lookupRename :: String -> LocalM (Maybe String)
lookupRename name = gets (foldl' step Nothing)
  where
    step (Just found) _ = Just found
    step Nothing scope = M.lookup name (scopeBindings scope)

isOuterUsed :: String -> LocalM Bool
isOuterUsed name = do
  globalNames <- lift (asks envGlobalNames)
  if name `S.member` globalNames
    then return True
    else gets (any (S.member name . scopeUsed))

bindName :: String -> String -> LocalM ()
bindName original renamed = modify \case
  [] -> [emptyScope {scopeBindings = M.singleton original renamed, scopeUsed = S.singleton renamed}]
  (s : rest) ->
    s
      { scopeBindings = M.insert original renamed (scopeBindings s),
        scopeUsed = S.insert renamed (scopeUsed s)
      }
      : rest

--------------------------------------------------------------------------------
-- Fresh names
--------------------------------------------------------------------------------

freshName :: Env -> ScopeStack -> String -> RenameM String
freshName env scopes base = do
  let used = envUsedNames env `S.union` foldMap scopeUsed scopes
      suffixes = [0 ..]
      candidate n = base ++ "_" ++ show n
      pick = head [candidate n | n <- suffixes, candidate n `S.notMember` used]
  lift (modify (+ 1))
  return pick

--------------------------------------------------------------------------------
-- Rename traversal
--------------------------------------------------------------------------------

renameDefinition :: Definition -> RenameM Definition
renameDefinition def = case def of
  FuncDef func loc -> FuncDef <$> renameFunc func <*> pure loc
  _ -> pure def

renameFunc :: Func -> RenameM Func
renameFunc func = case func of
  Func ds ident decl params items loc -> do
    (params', items') <- evalStateT (withScope (do
      params' <- renameParams params
      items' <- renameItems items
      return (params', items')
      )) []
    return (Func ds ident decl params' items' loc)
  OldFunc ds ident decl ids params items loc -> do
    (ids', params', items') <- evalStateT (withScope (do
      (ids', params') <- renameOldParams ids params
      items' <- renameItems items
      return (ids', params', items')
      )) []
    return (OldFunc ds ident decl ids' params' items' loc)

renameParams :: Params -> LocalM Params
renameParams (Params params isVariadic loc) = do
  params' <- mapM renameParam params
  return (Params params' isVariadic loc)

renameOldParams :: [Id] -> Maybe [InitGroup] -> LocalM ([Id], Maybe [InitGroup])
renameOldParams ids params = do
  ids' <- mapM renameId ids
  params' <- mapM renameInitGroups params
  return (ids', params')

renameParam :: Param -> LocalM Param
renameParam param = case param of
  Param (Just ident) declspec decl loc -> do
    ident' <- renameId ident
    return (Param (Just ident') declspec decl loc)
  _ -> return param

renameItems :: [BlockItem] -> LocalM [BlockItem]
renameItems = mapM renameBlockItem

renameBlockItem :: BlockItem -> LocalM BlockItem
renameBlockItem item = case item of
  BlockDecl group -> BlockDecl <$> renameInitGroup group
  BlockStm stmt -> BlockStm <$> renameStmt stmt
  _ -> return item

renameStmt :: Stm -> LocalM Stm
renameStmt stmt = case stmt of
  Block items loc -> do
    items' <- withScope (renameItems items)
    return (Block items' loc)
  If cond t mElse loc -> do
    cond' <- renameExp cond
    t' <- withScope (renameStmt t)
    mElse' <- mapM (withScope . renameStmt) mElse
    return (If cond' t' mElse' loc)
  While cond body loc -> do
    cond' <- renameExp cond
    body' <- withScope (renameStmt body)
    return (While cond' body' loc)
  DoWhile body cond loc -> do
    body' <- withScope (renameStmt body)
    cond' <- renameExp cond
    return (DoWhile body' cond' loc)
  For init' cond step body loc -> withScope $ do
    (initRenamed, cond', step') <- renameFor init' cond step
    body' <- renameStmt body
    return (For initRenamed cond' step' body' loc)
  Switch cond body loc -> do
    cond' <- renameExp cond
    body' <- renameStmt body
    return (Switch cond' body' loc)
  Case cond body loc -> do
    cond' <- renameExp cond
    body' <- renameStmt body
    return (Case cond' body' loc)
  CaseRange a b body loc -> do
    a' <- renameExp a
    b' <- renameExp b
    body' <- renameStmt body
    return (CaseRange a' b' body' loc)
  Default body loc -> Default <$> renameStmt body <*> pure loc
  Label ident attrs body loc -> do
    body' <- renameStmt body
    return (Label ident attrs body' loc)
  Exp expr loc -> Exp <$> mapM renameExp expr <*> pure loc
  Return expr loc -> Return <$> mapM renameExp expr <*> pure loc
  _ -> return stmt

renameFor :: Either InitGroup (Maybe Exp) -> Maybe Exp -> Maybe Exp -> LocalM (Either InitGroup (Maybe Exp), Maybe Exp, Maybe Exp)
renameFor init' cond step = do
  initRenamed <- case init' of
    Left group -> Left <$> renameInitGroup group
    Right expr -> Right <$> mapM renameExp expr
  cond' <- mapM renameExp cond
  step' <- mapM renameExp step
  return (initRenamed, cond', step')

renameInitGroups :: [InitGroup] -> LocalM [InitGroup]
renameInitGroups = mapM renameInitGroup

renameInitGroup :: InitGroup -> LocalM InitGroup
renameInitGroup group = case group of
  InitGroup declspec attrs inits loc -> do
    inits' <- mapM renameInit inits
    return (InitGroup declspec attrs inits' loc)
  _ -> return group

renameInit :: Init -> LocalM Init
renameInit (Init ident decl asm initVal attrs loc) = do
  ident' <- renameId ident
  initVal' <- mapM renameInitializer initVal
  return (Init ident' decl asm initVal' attrs loc)

renameInitializer :: Initializer -> LocalM Initializer
renameInitializer initVal = case initVal of
  ExpInitializer expr loc -> ExpInitializer <$> renameExp expr <*> pure loc
  CompoundInitializer items loc -> do
    items' <- mapM (\(design, init2) -> (,) design <$> renameInitializer init2) items
    return (CompoundInitializer items' loc)
  _ -> return initVal

renameExp :: Exp -> LocalM Exp
renameExp expr = case expr of
  Var ident loc -> Var <$> renameIdUsage ident <*> pure loc
  BinOp op a b loc -> BinOp op <$> renameExp a <*> renameExp b <*> pure loc
  Assign a op b loc -> Assign <$> renameExp a <*> pure op <*> renameExp b <*> pure loc
  PreInc a loc -> PreInc <$> renameExp a <*> pure loc
  PostInc a loc -> PostInc <$> renameExp a <*> pure loc
  PreDec a loc -> PreDec <$> renameExp a <*> pure loc
  PostDec a loc -> PostDec <$> renameExp a <*> pure loc
  UnOp op a loc -> UnOp op <$> renameExp a <*> pure loc
  SizeofExp a loc -> SizeofExp <$> renameExp a <*> pure loc
  Cast ty a loc -> Cast ty <$> renameExp a <*> pure loc
  Cond a b c loc -> Cond <$> renameExp a <*> renameExp b <*> renameExp c <*> pure loc
  Member a ident loc -> Member <$> renameExp a <*> pure ident <*> pure loc
  PtrMember a ident loc -> PtrMember <$> renameExp a <*> pure ident <*> pure loc
  Index a b loc -> Index <$> renameExp a <*> renameExp b <*> pure loc
  FnCall a args loc -> FnCall <$> renameExp a <*> mapM renameExp args <*> pure loc
  CudaCall a conf args loc -> CudaCall <$> renameExp a <*> pure conf <*> mapM renameExp args <*> pure loc
  Seq a b loc -> Seq <$> renameExp a <*> renameExp b <*> pure loc
  CompoundLit ty items loc -> do
    items' <- mapM (\(design, init2) -> (,) design <$> renameInitializer init2) items
    return (CompoundLit ty items' loc)
  StmExpr items loc -> do
    items' <- withScope (renameItems items)
    return (StmExpr items' loc)
  _ -> return expr

renameId :: Id -> LocalM Id
renameId (Id name loc) = do
  alreadyUsed <- isOuterUsed name
  scopes <- get
  if alreadyUsed
    then do
      env <- lift ask
      newName <- lift (freshName env scopes name)
      registerUsed newName
      bindName name newName
      return (Id newName loc)
    else do
      registerUsed name
      bindName name name
      return (Id name loc)
renameId ident = return ident

renameIdUsage :: Id -> LocalM Id
renameIdUsage (Id name loc) = do
  renamed <- lookupRename name
  return $ Id (fromMaybe name renamed) loc
renameIdUsage ident = return ident
