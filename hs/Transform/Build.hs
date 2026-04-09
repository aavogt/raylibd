{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Transform.Build
  ( buildStateSpec,
    collectGlobalVars,
    collectStaticLocals,
    toStateFields,
    toInitStmts,
    toUseRewrite,
    isWhile,
    dropMainNonStatic,
  )
where

import Control.Lens hiding (Const)
import Control.Lens.Extras
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Language.C hiding (mkIdent)
import Language.C.Quote.C
import Transform.Common

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
      isNonConstSpec specs,
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
        isNonConstSpec specs,
        fn == "main" || isStaticSpec specs,
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
          useExpr = mkMemberFrom s (fieldName field)
        }

isWhile :: BlockItem -> Bool
isWhile (BlockStm (While {})) = True
isWhile _ = False

-- TODO partition into (declaration, definition)
dropMainNonStatic :: StateSpec -> [Definition] -> [Definition]
dropMainNonStatic spec = applyRewrites (useRewrite spec (mkIdent "s_top")) . filter (\d -> all ($ d) [notMain, notInit, notTypedef])
  where
    notTypedef (DecDef TypedefGroup {} _) = False
    notTypedef _ = True

    notMain (FuncDef (Fun "main" _) _) = False
    notMain _ = True

    notInit (DecDef (InitGroup specs _ inits _) _) = not (isNonConstSpec specs) || isStaticSpec specs
    notInit _ = True
