module Transform.BuildSpec where

import Test.Hspec
import qualified Test.Hspec.Golden
import Transform.Build
import Language.C.Quote.C
import Text.PrettyPrint.Mainland.Class
import Text.PrettyPrint.Mainland

spec :: Spec
spec = return ()


-- dropMainNonStatic :: StateSpec -> [Definition] -> [Definition]
test1 = do
  let d = [cunit|
          const int c = 1;
          int v = 2;
          static int z = 3;
          void main() {}
          void nmain() {}
          static const int sci = 4;  |]
      s = buildStateSpec d
  putStrLn $ pretty 120 $  ppr $ dropMainNonStatic s d
