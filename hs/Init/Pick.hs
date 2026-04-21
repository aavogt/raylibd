module Init.Pick where

import Control.DeepSeq (NFData)
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Char
import Data.Data
import Data.IORef
import Data.List
import Data.Ord
import Data.Foldable
import GHC.Generics
import Init.PickAlign
import Paths_raylibd
import System.Console.ANSI
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import Test (runWithStdin)
import Text.Read
import System.Timeout

newtype Picked = Picked [(String, String)] deriving (Typeable, Generic, Show)

copyPicked :: FilePath -> Picked -> IO ()
copyPicked destPrefix (Picked sds) = for_ sds \ (src, dest) -> copyFile src (destPrefix </> dest)

instance NFData Picked

instance Exception Picked

testPicker = do
  print =<< runWithStdin "basic\ESC[A\n" picker
  print =<< runWithStdin "basic\ESC[5~\n" picker
  print =<< runWithStdin "basic\ESC[5~\ESC[B\n" picker
  print =<< runWithStdin "bas\DEL\DEL\DELtur\n" picker
  print =<< runWithStdin "basicin\n" picker
  return True

picker :: IO (Maybe Picked)
picker = do
  either Just (const Nothing) <$> try do
    d <- getDataFileName "init"
    dc <- getDirectoryContents d
    let dcc = sortByDigits $ filter ((== ".c") . takeExtensions) dc
    let ndcc = length dcc
    putStrLn "======== Templates ========"
    mapM_ putStrLn dcc
    putStr $
      setSGRCode [SetColor Foreground Vivid Black]
        ++ "> fuzzy match accept with Enter"
        ++ resetCode
    hFlush stdout
    nsRef <- newIORef ""
    highlighted <- newIORef (max 0 (ndcc - 1))
    let runFst :: (Monad m) => m (m a, b) -> m (a, b)
        runFst mmab = _1 id =<< mmab
        withNsRef k = runFst $ atomicModifyIORef nsRef k
        clamp lo hi val = max lo (min hi val)
        lastIndex = max 0 (ndcc - 1)
        moveHighlight delta = modifyIORef' highlighted \i -> clamp 0 lastIndex (i + delta)
        setHighlight idx = writeIORef highlighted (clamp 0 lastIndex idx)
    forever do
      code <- getCode
      case code of
        Left "\ESC[A" -> moveHighlight (-1)
        Left "\ESC[B" -> moveHighlight 1
        Left "\ESC[5~" -> setHighlight 0
        Left "\ESC[6~" -> setHighlight lastIndex
        _ -> pure ()
      (enter, ns) <-
        case code of
          Right n ->
            withNsRef \ns ->
              let def f =
                    let nns | isPrint n = n : f ns | otherwise = f ns
                     in (nns, (return False :: IO Bool, reverse nns))
               in case n of
                    '\DEL' -> def (drop 1) -- backspace
                    '\ETB' -> def dropWord -- \^W
                    '\n' -> def id & _2 . _1 .~ return True
                    _ -> def id & _2 . _1 .~ do unless (isPrint n) (putStr "\BEL"); return False
          _ -> do
            ns <- readIORef nsRef
            return (False, reverse ns)
      let dccDists = fst . alignScore ns <$> dcc
      let dccSorted = sortBy (comparing fst) (zip dccDists dcc)
      let ranked = reverse dccSorted

      clearLine
      replicateM_ ndcc do
               cursorUpLine 1
               clearLine

      focusIndex <- clamp 0 (length ranked - 1) <$> readIORef highlighted
      for_ (map (focusIndex==) [0 ..] `zip` ranked) \ (cyan,entry) ->
            putStrLn $ (if cyan
               then setSGRCode [SetColor Foreground Vivid Cyan] ++ "> " ++ resetCode
               else "  ")
              ++ renderRank ns entry

      putStr ('>' : ns)
      hFlush stdout
      when (enter && not (null ranked)) do
          let digits = takeWhile (/= '-') $ snd (ranked !! focusIndex)
              srcs = filter ((digits `isPrefixOf`) . takeFileName) dc
              -- duplicates checked by scripts/check-init.sh
              toMain p | takeExtension p == ".c" = "main.c" | otherwise = p
          throwIO $ Picked [ (d </> src, toMain (takeFileName src)) | src <- srcs ]

getCode :: IO (Either String Char)
getCode = do
  n <- getChar
  if n /= '\ESC'
    then pure (Right n)
    else do
      next <- timeout 20000 (hLookAhead stdin)
      case next of
        Just '[' -> do
          _ <- getChar
          tailCode <- readEscTail ""
          pure (Left ("\ESC[" ++ tailCode))
        _ -> pure (Right '\ESC')
  where
    readEscTail acc = do
      c <- getChar
      let nacc = c:acc
      if c == '~' || isAlpha c
        then pure (reverse nacc)
        else readEscTail nacc

-- | almost terminal ^W behavior (where the chars are reversed): drop the prefix with equal isAlphaNum
dropWord :: [Char] -> [Char]
dropWord (span isAlphaNum -> (a, b))
  | null a = dropWhile (not . isAlphaNum) b
  | otherwise = b

pickByIndex :: Int -> IO (Maybe Picked)
pickByIndex idx = do
  d <- getDataFileName "init"
  dc <- getDirectoryContents d
  let dcc = filter ((== ".c") . takeExtensions) dc
      matches = filter (matchesIndex idx) dcc
      toMain p | takeExtension p == ".c" = "main.c" | otherwise = p
  case matches of
    [] -> die $ "No init template matching index " ++ show idx
    _ -> return $ Just $ Picked [ (d </> src, toMain (takeFileName src)) | src <- matches ]
  where
    matchesIndex n name =
      case takeWhile isDigit name of
        "" -> False
        digits -> readMaybe digits == Just n

printList :: IO ()
printList = do
  d <- getDataFileName "init"
  dc <- getDirectoryContents d
  let dcc = sortByDigits $ filter ((== ".c") . takeExtensions) dc
  mapM_ putStrLn dcc

sortByDigits files = files
    & map (\x -> (read (takeWhile isDigit x) :: Int, x))
    & sortOn fst
    & map snd

renderRank :: String -> (Int, String) -> String
renderRank needle (_cost, haystack) =
  let (score, tags) = alignScore needle haystack
   in renderTagged haystack tags

renderTagged :: String -> [Tag] -> String
renderTagged haystack tags = renderParts (zip haystack tags)
  where
    renderParts [] = ""
    renderParts ((c, tag) : rest) =
      colorFor tag ++ [c] ++ resetCode ++ renderParts rest

colorFor :: Tag -> String
colorFor tag =
  case tag of
    TagMatch -> setSGRCode [SetColor Foreground Vivid Green]
    TagSub -> setSGRCode [SetColor Foreground Vivid Yellow]
    TagIns -> ""

resetCode :: String
resetCode = setSGRCode [Reset]
