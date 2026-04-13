import Control.Concurrent
import Control.Lens
import Control.Monad
import qualified Data.ByteString.Char8 as C8
import Data.Foldable
import Data.Functor ((<&>))
import Data.IORef
import Data.List
import Data.Loc
import Language.C hiding (Init)
import ParseTD
import Paths_raylibd
import System.Console.CmdArgs
import System.Directory
import System.Exit
import System.FSNotify
import System.FilePath
import System.IO
import System.Process
import Text.Show.Pretty
import Transform

data Raylibd
  = Raylibd {inputmain, outputmain :: String, cflags, cflags_extra, typedefs, typedefs_extra :: [String], echo :: Bool, once :: Bool}
  | Init {inputmain :: String, dest :: Maybe FilePath, force :: Bool}
  deriving (Data)

watchmode =
  Raylibd
    { inputmain = "main.c" &= opt "main.c" &= typ "FILE" &= argPos 0,
      outputmain = "dll.c",
      cflags = ["-std=gnu17", "-DRAYLIBD=0"] &= help "c preprocessor flags",
      cflags_extra = [],
      typedefs =
        words
          "bool Vector2  Vector3  Vector4  Matrix  Color  Rectangle  Image  Texture  RenderTexture  NPatchInfo  GlyphInfo  Font  Camera2D  Camera3D \
          \ Shader  MaterialMap  Material  Mesh  Model  ModelAnimation  Transform  BoneInfo  Ray  RayCollision  BoundingBox  Wave  AudioStream  \
          \ Sound  Music  VrDeviceInfo  VrStereoConfig  FilePathList  AutomationEvent  AutomationEventList RenderTexture2D"
          &= help "override raylib typedef struct",
      typedefs_extra = [] &= help "extra c typedefs for example --typedefs-extra=VAR,uint8_t,Expredges",
      echo = False &= help "echo the generated file to stdout",
      once = False &= help "don't watch"
    }
    &= help "turn [FILE] into dll.c which is a "

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
  for_ dest setCurrentDirectory
  system "make compile_commands.json"
watch Raylibd {..} = withManagerConf defaultConfig {confDebounce = Debounce 0.1} \mgr -> do
  dir <- takeDirectory <$> makeAbsolute inputmain
  let cppFlags = "-MM" : cflags ++ cflags_extra
  includes <-
    runPreprocessor "gcc" cppFlags inputmain <&> \case
      Left _exit -> [inputmain]
      Right bs -> map C8.unpack $ drop 1 $ C8.words bs

  -- beginning of the file the #endif matching the #ifndef RAYLIBD inclusive
  includes_prop <-
    let takeUntilEndif n seen = \case
          "#endif" : _ | n == 1 -> ["#endif"]
          x : xs ->
            let seen' = seen || (x == "#ifndef RAYLIBD")
             in x : takeUntilEndif (adjustN x n seen') seen' xs
        adjustN "#endif" n True = n - 1
        adjustN c n True | "#if" `isPrefixOf` c = n + 1
        adjustN _ n _ = n
     in readFile inputmain <&> unlines . takeUntilEndif 0 False . lines

  prev <- newIORef (Prev Nothing [])
  templatec <- getDataFileName "template.c"
  let reloadMainC = do
        let allTypedefs = concatMap split (typedefs ++ typedefs_extra)
        result@(~(Right from)) <- parseCFileWithGcc "gcc" (cflags ++ cflags_extra) allTypedefs inputmain
        case result of
          Left e -> hPrint stderr result
          _ -> return ()

        let spec = buildStateSpec from
        sub <- atomicModifyIORef' prev (substituteTemplate from spec)
        subbed <- sub . (includes_prop ++) <$> readFile templatec
        writeFile outputmain subbed
        when echo $ putStrLn subbed

  reloadMainC
  when once exitSuccess
  ch <- newChan
  watchTreeChan mgr dir (\p -> takeFileName (eventPath p) `elem` includes && notRemoved p) ch
  forever do
    ev <- readChan ch
    reloadMainC

split :: String -> [String]
split [] = [[]]
split (x : xs)
  | x `elem` " ,;" = [] : split xs
  | otherwise = split xs & _head %~ (x :)

runPreprocessor :: FilePath -> [String] -> FilePath -> IO (Either ExitCode C8.ByteString)
runPreprocessor gcc flags input = do
  (exit, out, _err) <- readProcessWithExitCode gcc (flags ++ [input]) ""
  pure $ case exit of
    ExitSuccess -> Right (C8.pack out)
    _ -> Left exit

parseCFileWithGcc gcc flags typedefs input = do
  let args = "-E" : flags ++ [input]
  whenLoud $ putStrLn $ unwords $ ">" : gcc : args
  (exit, out, _err) <- readProcessWithExitCode gcc args ""
  let contents = case exit of
        ExitSuccess -> out
        _ -> ""
  case parseAddingTD [] typedefs parseUnit (C8.pack contents) (Just (Pos input 1 1 0)) of
    ([], r) -> return r
    (inferredTypedefs, r) -> do
      whenLoud $ pPrint ("new typedefs:", inferredTypedefs)
      return r

notRemoved = \case
  Removed {} -> False
  _ -> True
