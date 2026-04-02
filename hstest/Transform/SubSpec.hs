{-# LANGUAGE BlockArguments #-}
module Transform.SubSpec where

import Transform.Sub
import Test.Hspec
import qualified Test.Hspec.Golden
import Language.C.Quote.C
import Transform
import Text.PrettyPrint.Mainland hiding (render)
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
gold s v = do
  env <- SpecM (lift ask)
  it s $ Test.Hspec.Golden.defaultGolden (sanitize $ intercalate "." (unsafeCoerce env) ++ "." ++ s) (pretty 1000 $ ppr v)

sanitize = mapMaybe \case
  '[' -> Just '_'
  ']' -> Just '_'
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

test3 = do
  let a = [cunit| float xs[2]; char n; |]
  let b = [cunit| float xs[3]; char n; |]
  let c = [cunit| float xs[4]; char n; |]
  let d = [cunit| float xs[3]; char n; |]
  let e = [cunit| float xs[2]; char n; |]

  let sa = buildStateSpec a
  let sb = buildStateSpec b
  let sc = buildStateSpec c
  let sd = buildStateSpec d
  let se = buildStateSpec e

  let fa = fields sa
  let fb = mergeSF (fields sa) (fields sb)
  let fc = fb `mergeSF` fields sc
  let fd = fc `mergeSF` fields sd
  let fe = fd `mergeSF` fields se
  putStrLn "struct state_a {"
  ppMembers fa
  putStrLn "} struct state_b {"
  ppMembers fb
  putStrLn "} struct state_c {"
  ppMembers fc
  putStrLn "} struct state_d {"
  ppMembers fd
  putStrLn "} struct state_e {"
  ppMembers fe
  putStrLn "}"

ppMembers = putStrLn . renderDecls . buildStateMembers . uniqueDummy
renderDecls xs = concatMap ((++ ";\n") . render) xs

render x = pretty 120 $ ppr x
renderBody stms = putStrLn $ render $ Block (map BlockStm stms) noLoc
showReinit prev spec = do
        putStrLn "-- ReinitAlloc"
        renderBody (reinitAllocStmts prev spec)
        putStrLn "-- ReinitInPlace"
        renderBody (reinitInPlaceStmts prev spec)

test4 = do
  let a = [cunit| float xs[2]; char n; |]
  let b = [cunit| float xs[3]; char n; |]
  let c = [cunit| float xs[4]; char n; |]
  let d = [cunit| float xs[3]; char n; |]
  let e = [cunit| float xs[2]; char n; |]

  let sa = buildStateSpec a
  let sb = buildStateSpec b
  let sc = buildStateSpec c
  let sd = buildStateSpec d
  let se = buildStateSpec e

  let fa = fields sa
  let fb = mergeSF (fields sa) (fields sb)
  let fc = fb `mergeSF` fields sc
  let fd = fc `mergeSF` fields sd
  let fe = fd `mergeSF` fields se

  putStrLn "====== a->b ======"
  showReinit (Just sa {fields = fa}) (sb {fields = fb})
  putStrLn "====== b->c ======"
  showReinit (Just sb {fields = fb}) (sc {fields = fc})
  putStrLn "====== c->d ======"
  showReinit (Just sc {fields = fc}) (sd {fields = fd})
  putStrLn "====== d->e ======"
  showReinit (Just sd {fields = fd}) (se {fields = fe})
