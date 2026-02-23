{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}


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
  -- putStrLn =<< readFile mainc
  writeFile "ref/rl.ast" (show $ over template (const undefNode) from)

  let spec = buildStateSpec from
      orig' = rewriteOrig spec from
      render x = show $ Language.C.pretty x
      renderDecls decls = concatMap ((++";") . render) decls
      structBody = renderDecls (buildStateMembers (fields spec))

      initBody = render (extractFuncBody "Init" orig')
      updateBody = render (extractFuncBody "Update" orig')
      uninitBody = render (extractFuncBody "Uninit" orig')

  subbed <- readFile "template.c" <&> lined %~ \case
    "//STRUCTBODY" -> structBody
    "//INITBODY" -> initBody
    "//UPDATEBODY" -> updateBody
    "//UNINITBODY" -> uninitBody
    x -> x

  writeFile "rl2.c" subbed
  putStrLn subbed


  system "sed -i 's/(OnlyPos <no file> (<no file>,-1))/POS/g' ref/rl.ast"

  -- https://keep.google.com/#NOTE/1UOr_JoLDwVFovuQDgZiYyDoTsIYbGk4EFqTpdxZuu_8V_cQ4NJ8o5GzyYLTOdUe682PP
  -- source-to-source transformation
  -- split main() into void Init(s); bool Update(s); void Unload(s)
  -- extract rename locals into a struct
  -- optionally make the render texture rt in wip/gcodeviewer/src/main.c transparent?
  -- bool Update isn't enough then?
  -- there needs to be a flag in game state s as to whether anything changed
  -- rt might not be needed instead call BeginDrawing();EndDrawing()
  -- as in the wip/gcodeviewer no_rt branch
  --
  -- similar to StaticVars.hs in
  -- https://gist.github.com/aavogt/2312f92289b1bcf65cd506b365949a56
  -- originally /home/aavogt/wip/climber/moonboard/beta_notation
  return ()
