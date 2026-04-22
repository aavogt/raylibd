{-# LANGUAGE OverloadedLists #-}

module Transform.Assets
  ( AssetFunc (..),
    assetFunctions,
    rewriteAssetLoads,
    assetLoadNames,
    wrappedName,
  )
where

import Control.Lens hiding (Const)
import Control.Lens.Extras
import Data.Data (Data)
import Data.Loc
import qualified Data.Set as S
import Language.C hiding (mkIdent)
import Language.C.Quote.C
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class (ppr)
import Text.Show.Pretty
import Data.Either
import Data.Monoid
import Control.Monad.Trans.Writer

-- | Description of one Load/Unload pair.
-- afTotalArgs is the total number of arguments the function takes.
-- afMaxPaths is how many of the leading args are file path strings.
data AssetFunc = AssetFunc
  { -- | e.g. "Shader"
    afType :: String,
    -- | e.g. "LoadShader"
    afLoad :: String,
    -- | e.g. "UnloadShader"
    afUnload :: String,
    -- | total parameter count
    afTotalArgs :: Int,
    -- | how many leading const char* path args
    afMaxPaths :: Int
  }
  deriving (Show)

assetFunctions :: [AssetFunc]
assetFunctions =
  [ AssetFunc s ("Load" ++ s) ("Unload" ++ s) n n
    | (n, s) <- zip (2 : repeat 1) (words "Shader Texture Image Model Font Sound Wave")
  ]

assetLoadNames :: S.Set String
assetLoadNames = S.fromList [afLoad af | af <- assetFunctions]

wrappedName :: String -> String
wrappedName name = "_rl_" ++ name

-- | Rewrite all calls to Load* functions in the AST so they go through
-- the wrapper that registers the asset.  Only renames the function
-- identifier at call sites; the wrapper definitions live in template.c.
rewriteAssetLoads :: (Data a) => a -> a
rewriteAssetLoads = (template %~ rewrite goe) . (template %~ rewrite goi)
  where
    goe :: Exp -> Maybe Exp
    goe (Assign v JustAssign (FnCall (Var (Id loadShader _) _) args _) _)
      | loadShader `S.member` assetLoadNames =
          Just $
            Assign
              v
              JustAssign
              (FnCall (Var (Id (wrappedName loadShader) noLoc) noLoc) (asArg v : args) noLoc)
              noLoc
    goe e = Nothing

    goi :: Init -> Maybe Init
    goi
      ( Init
          v
          (DeclRoot _)
          Nothing
          ( Just
              ( ExpInitializer
                  ( FnCall
                      (Var (Id loadShader _) _)
                      args
                      _
                    )
                  _
                )
            )
          []
          _
        )
        | loadShader `S.member` assetLoadNames =
            Just $
              Init
                v
                (DeclRoot noLoc)
                Nothing
                ( Just
                    ( ExpInitializer
                        ( FnCall
                            (Var (Id (wrappedName loadShader) noLoc) noLoc)
                            (asArg v : args)
                            noLoc
                        )
                        noLoc
                    )
                )
                []
                noLoc
    goi
      ( Init
          v
          array
          Nothing
          ( Just
              ( CompoundInitializer
                  (rewrittenCompoundInitializers v -> Just initializers)
                  _
                )
            )
          []
          _
        ) =
        Just
          ( Init
              v
              array
              Nothing
              ( Just
                  ( CompoundInitializer
                      initializers
                      noLoc
                  )
              )
              []
              noLoc
          )
    goi _ = Nothing

asArg v = Const (StringConst [pv] pv noLoc) noLoc
  where
    pv = pretty 10 (ppr v)

rewrittenCompoundInitializers v = asMaybe . (traversed . _2 %%~ f)
  where
  -- prevent an infinite loop by telling `rewrite`
  -- that `f` didn't make any changes
  asMaybe w = case runWriter w of
    (r, Any True) -> Just r
    _ -> Nothing

  f (ExpInitializer (FnCall (Var (Id loadShader _) _) args _) _)
      | loadShader `S.member` assetLoadNames = do
          tell (Any True)
          return
            ( ExpInitializer
                ( FnCall
                    (Var (Id (wrappedName loadShader) noLoc) noLoc)
                    (asArg v : args)
                    noLoc
                )
                noLoc
            )
  f x = return x

instance Plated Exp

instance Plated Init

-- TODO:
-- Shader s4[2] = {_rl_LoadShader(s4, a, b), _rl_LoadShader(s4, c, d)};
-- all s4 should be s4[2]
test1 = do
  let s =
        [cunit| void main() { s1 = LoadShader(a,b);
    typename Shader s2 = LoadShader(a,b), s3 = LoadShader(b, a);
    typename Shader s4[2] = { LoadShader(a,b), LoadShader(c,d), }; } |]
  putStrLn $ pretty 100 $ ppr (rewriteAssetLoads s)
  return False
