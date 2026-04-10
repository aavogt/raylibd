{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-typed-holes -fdefer-typed-holes #-}

-- | assets and shaders
-- does music resume?
module Transform.Assets (module Transform.Assets) where

import Control.Applicative (Alternative (..))
import Control.Lens hiding (Const)
import Control.Lens.Extras
import Control.Monad (MonadPlus (mplus), guard)
import Control.Monad.Trans.State
import Data.Data (Data)
import Data.Generics (Data, Typeable)
import Data.Generics.Zipper
import Data.List
import Data.Loc (noLoc)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Set as Set
import Language.C hiding (mkIdent)
import Language.C.Quote.C
import Language.C.Syntax
import Text.Show.Pretty

-- * zyb extras

-- Generic "collect all matching zipper positions" since syz lacks this directly
collectDR :: (Data root) => (Zipper root -> Maybe b) -> Zipper root -> [b]
collectDR f z =
  maybeToList (f z)
    ++ do
      c <- maybeToList (down' z)
      collectDR f c ++ siblings c
  where
    siblings c = do
      d <- maybeToList (right c)
      collectDR f d ++ siblings d

collectUL :: (Data root) => (Zipper root -> Maybe b) -> Zipper root -> [b]
collectUL f z =
  maybeToList (f z)
    ++ do
      c <- maybeToList (up z)
      collectUL f c ++ siblings c
  where
    siblings c = do
      d <- maybeToList (left c)
      collectUL f d ++ siblings d

-- Navigate up through heterogeneous parents until we hit a node of type b
upUntil :: (Typeable b) => Zipper root -> Maybe (Zipper root, b)
upUntil z = do
  p <- up z
  case getHole p of
    Just v -> Just (p, v)
    Nothing -> upUntil p

-- TODO: Chase a variable argument to a StringConst
stringConstsOf :: (Data root) => Zipper root -> Exp -> Maybe String
stringConstsOf z (Const (StringConst _ s _) _) = Just s
stringConstsOf z (Var (Id n _) _) = _ $ chase z n

chase z n = listToMaybe $ collectUL fs z
  where
    fs y = initingStr n (getHole y) <|> (chase z =<< assignedFrom n (getHole y))

-- $> initingStr "xs" ([cunit| int xs[] = "ini"; |] ^? template)
-- Just "ini"

initingStr n = initingStrP (n ==)

initingStrP p w = do
  Init (Id n' _) _ _ (Just (ExpInitializer (Const (StringConst _ s _) _) _)) _ _ <- w
  guard (p n')
  Just s

-- $> assignedFrom "xs" ([cunit| void main() { xs = ys; } |] ^? template)
-- Just "ys"

assignedFrom n = assignedFromP (n ==)

assignedFromP p w = do
  Assign (Var (Id a _) _) _ (Var (Id b _) _) _ <- w
  guard (p a)
  Just b

valUpLeft :: (Data root) => String -> Zipper root -> Maybe String
valUpLeft var z = listToMaybe $ collectUL _ z

-- how much indirection to support? it's not adversarial
indirections = do
  let ex = [cunit| char xs[] = "name"; char ys[10]; char *zs = xs; void main() { memcpy(ys, "name2"); snprintf(); } |]
  -- pPrint ex

  pPrint [cunit| void main() { memcpy(xs, ys); } |]

data CallContext = CallContext
  { ccArgs :: [Exp], -- arguments (left)
    ccAssignTo :: Maybe Exp, -- lhs of assignment, if any (right)
    ccArgConsts :: [(Exp, Maybe String)] -- per-arg StringConst leaves
  }
  deriving (Show)

findCallContexts :: forall root. (Data root) => Set String -> root -> [CallContext]
findCallContexts fnnames root = collectDR matchFnCall (toZipper root)
  where
    matchFnCall :: Zipper root -> Maybe CallContext
    matchFnCall z
      | Just (FnCall (Var (Id ccName _) _) ccArgs _) <- getHole z,
        -- \| Just [cexp| $id:ccName($params:ccArgs); |]<- getHole z,
        -- there is a PatQ quasiquoter but it doesn't seem to bind variables
        ccName `Set.member` fnnames =
          let ccAssignTo = do
                (_, Assign lhs _ e _) <- upUntil z
                guard (Just e == getHole z)
                Just lhs
              ccArgConsts = [(a, stringConstsOf z a) | a <- ccArgs]
           in Just CallContext {..}
      | otherwise = Nothing

-- ** strings in AST

shaderExt = words "fs vs glsl frag vert"

assetExt = words "png jpg obj gltf ttf wav ogg"

fsvs = do
  let shaderProg =
        [cunit| char sp[] = "s.fs";
    typename Shader u() {
      const char uvs = "u.vs";
      static int loaded = 0;
      static typename Shader s;
      if (!loaded) {
      s = LoadShader(uvs, "u.fs");
      loaded = true;
      }
      return s;
    }
    void main() {
      typename Shader s = LoadShader("s.vs", sp);
      typename Shader t = LoadShader("t.vs", "t.fs"), u = u();
    }
    |]
  pPrint $ findCallContexts ["LoadShader"] shaderProg
