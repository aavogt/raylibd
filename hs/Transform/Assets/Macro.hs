module Transform.Assets.Macro where
import Transform.Assets
import Data.List
import PyF

assetWrappersC :: String
assetWrappersC = unlines $ zipWith genDefine [0..] assetFunctions
  where
    genDefine (kindIdx :: Int) (AssetFunc {..}) =
      let a0:args = ["a" ++ show i | i <- [0 .. afTotalArgs]]
          sep = intercalate ", "
          regHelper = "_rl_register_asset_" ++ show afMaxPaths
       in [fmt|#define {wrappedName afLoad}(a0, {sep args}) \\
                {regHelper}({kindIdx}, \\
                offsetof(struct state, a0), \\
                {afLoad}({sep args}), \\
                {sep (take afMaxPaths args)})|]

assetReloadSwitchKind = unlines  $ zipWith f [0 :: Int .. ] assetFunctions

f :: Int -> AssetFunc -> String
f k AssetFunc{.. } = [fmt|
  case {k}: {{
    dest = (void*)s + s->_assets[_i].field_offset;
    {afUnload}(*({afType}*)dest);
    *({afType}*)dest = {afLoad}({args});
    break;
  }}|]
  where args = intercalate ", " [ [fmt| args[{i}] |] | i <- [0 .. afMaxPaths -1] ]
