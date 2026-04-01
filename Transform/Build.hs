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
    mergeSF,
    isWhile,
    dropMainNonStatic,
  )
where

import Control.Lens hiding (Const)
import Control.Lens.Extras
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
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
          useExpr = mkMemberFrom s (fieldName field)
        }

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

    notInit (DecDef (InitGroup specs _ inits _) _) = isStaticSpec specs
    notInit _ = True
