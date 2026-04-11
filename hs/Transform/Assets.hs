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
import Data.Data (Data)
import qualified Data.Set as S
import Control.Lens.Extras
import Language.C hiding (mkIdent)
import Data.Loc
import Language.C.Quote.C
import Text.PrettyPrint.Mainland.Class (ppr)
import Text.PrettyPrint.Mainland

-- | Description of one Load/Unload pair.
-- afTotalArgs is the total number of arguments the function takes.
-- afMaxPaths is how many of the leading args are file path strings.
data AssetFunc = AssetFunc
  { afType :: String, -- ^ e.g. "Shader"
    afLoad :: String,     -- ^ e.g. "LoadShader"
    afUnload :: String,   -- ^ e.g. "UnloadShader"
    afTotalArgs :: Int,   -- ^ total parameter count
    afMaxPaths :: Int     -- ^ how many leading const char* path args
  }
  deriving (Show)

assetFunctions :: [AssetFunc]
assetFunctions = [ AssetFunc s ("Load" ++ s) ("Unload"++s) n n
    | (n, s) <- zip (2:repeat 1) (words "Shader Texture Image Model Font Sound Wave") ]

assetLoadNames :: S.Set String
assetLoadNames = S.fromList [afLoad af | af <- assetFunctions]

wrappedName :: String -> String
wrappedName name = "_rl_" ++ name

-- | Rewrite all calls to Load* functions in the AST so they go through
-- the wrapper that registers the asset.  Only renames the function
-- identifier at call sites; the wrapper definitions live in template.c.
rewriteAssetLoads :: (Data a) => a -> a
rewriteAssetLoads = template %~ rewrite go
  where
    go :: Exp -> Maybe Exp
    go (Assign v JustAssign (FnCall (Var (Id name _) _) args _) _)
      | name `S.member` assetLoadNames = Just $ Assign v JustAssign (FnCall (Var (Id (wrappedName name) noLoc) noLoc) (a:args) noLoc) noLoc
        where
        pv  = pretty 10 (ppr v)
        a = Const (StringConst [pv] pv noLoc) noLoc
    go e = Nothing

instance Plated Exp

test1 = putStrLn $ pretty 100 $ ppr $ rewriteAssetLoads [cunit| void main() { sh[2] = LoadShader(a,b) ; } |]
