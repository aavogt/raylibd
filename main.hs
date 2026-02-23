{-# LANGUAGE DeriveDataTypeable #-}


import Control.Lens
import Control.Lens.Plated
import Control.Monad
import Data.Data.Lens
import Data.Functor ((<&>))

import Language.C
import Language.C.System.GCC
import System.Console.CmdArgs
import System.FSNotify
import System.Process
import Text.Show.Pretty
import Transform


data Raylibd = Raylibd {mainc :: String, cflags :: [String]}
  deriving (Data)

main = do
  -- withManager $ \mgr -> do
  Raylibd {..} <- cmdArgs $ Raylibd {mainc = "rl.c" &= opt "rl.c" &= typ "INPUT" &= argPos 0, cflags = ["--std=c99", "-DRAYLIBD=0"]}
  Right from <- parseCFile (newGCC "gcc") (Just "/tmp") cflags mainc -- (initPos mainc)
  writeFile "ref/rl.ast" (show $ over template (const undefNode) from)

  subbed <- substituteTemplate from Nothing <$> readFile "template.c"
  writeFile "rl2.c" subbed
  putStrLn subbed

  system "sed -i 's/(OnlyPos <no file> (<no file>,-1))/undefNode/g' ref/rl.ast"
  return ()
