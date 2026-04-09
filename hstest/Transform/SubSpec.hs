{-# LANGUAGE BlockArguments #-}
module Transform.SubSpec where

import Transform.Sub
import Transform.MergeSF
import Test.Hspec
import qualified Test.Hspec.Golden
import Language.C.Quote.C
import Transform
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class
import Data.Maybe
import Language.C.Quote
import Data.Loc
import Test.Hspec.Core.Spec
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Data.List
import Unsafe.Coerce
import Text.Show.Pretty

-- https://github.com/stackbuilders/hspec-golden/issues/64
gold s v = goldText s (pretty 1000 (ppr v))

goldText :: String -> String -> Spec
goldText s v = do
  env <- SpecM (lift ask)
  it s $ Test.Hspec.Golden.defaultGolden (sanitize $ intercalate "." (unsafeCoerce env) ++ "." ++ s) v

goldSF :: String -> [StateField] -> Spec
goldSF s v = goldText s (renderDecls $ buildStateMembers $ uniqueDummy v)

renderDecls :: Pretty a => [a] -> String
renderDecls xs = concatMap ((++ ";\n") . pretty 120 . ppr) xs

sanitize = mapMaybe \case
  '[' -> Just '_'
  ']' -> Just '_'
  '-' -> Nothing
  '>' -> Just '_'
  ' ' -> Just '_'
  x -> Just x

spec :: Spec
spec = do
  describe "array[10]" do
    let fn = [cunit| void f() { while (true) { static int array[10]; } } |]
        spec = buildStateSpec fn
    gold "language-c-quote AST" fn
    gold "structdef" $ buildStateMembers (fields spec)

  describe "array[10][7]" do
    let fn = [cunit| void f() { while (true) { static int array[10][7]; } } |]
        spec = buildStateSpec fn
    gold "language-c-quote AST" fn
    gold "structdef" $ buildStateMembers (fields spec)

  describe "genloopsST" do
    gold "" $ genLoopsST "x" [1, 5]

  describe "toDecl" do
    gold "" $ map (fromMaybe (head [cunit|  void dropped(); |]) . toDecl)
          [cunit|
          typedef struct { float x, y; } Vector2;
          typedef struct {
                          Vector2 a, b;
                          typename bool placed[2];
                        } Seg;
          Seg segs[2];
          void dropped2(void); // should be?
          void mainly(int argc, char** argv) {}
          void main() {}
          void main(int argc, char** argv) {}
          void f(void) { printf(); }
          void g() { printf(); }
          |]

  describe "dropconst" do
      let const = [cunit| const float x, y; double z; |]
      gold "ast" const
      gold "onlyz" $ buildStateMembers $ fields (buildStateSpec const)

  describe "mergeSF" do
    let a = [cunit| float xs[2]; char n; |]
        b = [cunit| float xs[3]; char n; |]
        c = [cunit| float xs[4]; char n; |]
        d = [cunit| float xs[3]; char n; |]
        e = [cunit| float xs[2]; char n; |]
        sa = buildStateSpec a
        sb = buildStateSpec b
        sc = buildStateSpec c
        sd = buildStateSpec d
        se = buildStateSpec e
        fa = fields sa
        fb = mergeSF (fields sa) (fields sb)
        fc = fb `mergeSF` fields sc
        fd = fc `mergeSF` fields sd
        fe = fd `mergeSF` fields se
    goldSF "state_a" fa
    goldSF "state_b" fb
    goldSF "state_c" fc
    goldSF "state_d" fd
    goldSF "state_e" fe

  let g s x y = gold s $ Block (map BlockStm (reinitAllocStmts (Just x) y)) noLoc
  describe "reinit" do
    let a = [cunit| float xs[2]; char n; |]
        b = [cunit| float xs[3]; char n; |]
        c = [cunit| float xs[4]; char n; |]
        d = [cunit| float xs[3]; char n; |]
        e = [cunit| float xs[2]; char n; |]
        sa = buildStateSpec a
        sb = buildStateSpec b
        sc = buildStateSpec c
        sd = buildStateSpec d
        se = buildStateSpec e
        fa = fields sa
        fb = mergeSF (fields sa) (fields sb)
        fc = fb `mergeSF` fields sc
        fd = fc `mergeSF` fields sd
        fe = fd `mergeSF` fields se
    g "a->b" sa {fields = fa} sb {fields = fb}
    g "b->c" sb {fields = fb} sc {fields = fc}
    g "c->d" sc {fields = fc} sd {fields = fd}
    g "d->e" sd {fields = fd} se {fields = fe}

  describe "mergeSF initializers" do
    let a = [cunit| float x = 1; |]
    let b = [cunit| float x = 2; |]
        sa = buildStateSpec a
        sb = buildStateSpec b
        fa = fields sa
        fb = mergeSF (fields sa) (fields sb)
        r = reinitInPlaceStmts (Just sa) sb{ fields = fb }
    g "x=1 changed to x=2" sa sb{ fields = fb }

test1 = do
  let m = [cunit| void f() { typename FILE *w1; FILE *w2; } void main() { typename FILE *x; static int z = 0; } |]
  pPrint m
  let ok s = all (`notElem` s) "w" && all (`elem` s) "xz"
  let s = renderDecls $ buildStateMembers $ fields $ buildStateSpec m
  putStrLn s
  print $ ok s
  putStrLn "but missing typename on output"
