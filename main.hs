import Control.Concurrent
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import Data.Functor ((<&>))
import Data.IORef
import Data.Maybe
import Language.C
import Language.C.System.GCC
import Language.C.System.Preprocess
import Paths_raylibd
import System.Console.CmdArgs
import System.Directory
import System.FSNotify
import System.FilePath
import System.IO
import Transform
import System.Exit

data Raylibd
  = Raylibd {inputmain, outputmain :: String, cflags, cflags_extra :: [String], echo :: Bool, once :: Bool}
  | Init {inputmain :: String, dest :: Maybe FilePath, force :: Bool}
  deriving (Data)

watchmode =
  Raylibd
    { inputmain = "main.c" &= opt "main.c" &= typ "FILE" &= argPos 0,
      outputmain = "dll.c",
      cflags = ["-std=gnu17", "-DRAYLIBD=0"] &= help "c preprocessor flags",
      cflags_extra = [],
      echo = True,
      once = False
    }
    &= help "turn [FILE] "

initmode =
  Init
    { inputmain = "main.c" &= typ "FILE" &= opt "main.c",
      dest = Nothing &= argPos 0,
      force = False &= help "overwrite files"
    }

main =
  watch =<< cmdArgs (modes [watchmode &= auto, initmode])

watch Init {..} = do
  let addDest = maybe id (</>) dest
  let copyData n = do
        p <- getDataFileName n
        let d = addDest n
        createDirectoryIfMissing True (takeDirectory d)
        e <- doesFileExist d
        when (not e || force) $ copyFile p d
  mapM_ copyData $ words "main.c main_hot.c Makefile vendor/Makefile"
watch Raylibd {..} = withManagerConf defaultConfig \mgr -> do
  let gcc = newGCC "gcc"
  dir <- takeDirectory <$> makeAbsolute inputmain
  includes <-
    runPreprocessor gcc (addExtraOption (cppFile inputmain) "-MM") <&> \case
      Left _exit -> [inputmain]
      Right bs -> map C8.unpack $ drop 1 $ C8.words bs

  includes_prop <- readFile inputmain <&> unlines . takeWhile ((== "#") . take 1) . lines

  prevSpecRef <- newIORef Nothing
  templatec <- getDataFileName "template.c"
  let reloadMainC = do
        result@(~(Right from)) <- parseCFile gcc (Just "/tmp") (cflags ++ cflags_extra) inputmain
        case result of
          Left e -> hPrint stderr result
          _ -> return ()
        let spec = buildStateSpec from
        prevSpec <- atomicModifyIORef' prevSpecRef (Just spec,)
        subbed <- substituteTemplate from spec prevSpec . (includes_prop ++) <$> readFile templatec
        writeFile outputmain subbed
        when echo $ putStrLn subbed

  reloadMainC
  when once exitSuccess
  ch <- newChan
  watchTreeChan mgr dir (\p -> takeFileName (eventPath p) `elem` includes && notRemoved p) ch
  forever do
    ev <- readChan ch
    reloadMainC

notRemoved = \case
  Removed {} -> False
  _ -> True
