{-# LANGUAGE TypeApplications #-}

module Test where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.List
import GHC.IO.Exception
import GHC.IO.Handle
import System.IO
import System.IO.Error
import System.Posix.IO

testRunWithStdin = do
  let txt = "basic\ntext going here342"
  x <- runWithStdin txt getContents
  return (x == txt)

testEIO :: IO Bool
testEIO = do
  let e = userError "boom IO"
  Left (SomeException se) <- try $ runWithStdin "inp" (void (throwIO e))
  return (show se == show e)

testEA :: IO Bool
testEA = do
  Left e <- try @SomeException $ runWithStdin "inp" (return $ error "boom eval" :: IO ())
  return ("boom eval" `isPrefixOf` show e)

getContents2 = do
  b <- isEOF
  if b then return [] else (:) <$> getChar <*> getContents2

runWithStdin :: (NFData a) => String -> IO a -> IO a
runWithStdin input action = do
  (readFd, writeFd) <- System.Posix.IO.createPipe
  _ <- forkIO $ do
    let writeAndClose = fdWrite writeFd input >> closeFd writeFd
    writeAndClose `catch` \e -> if isResourceVanishedError e then closeFd writeFd else throwIO e
  savedFd <- dup stdInput
  dupTo readFd stdInput
  closeFd readFd
  savedBuffering <- hGetBuffering stdin
  hSetBuffering stdin LineBuffering
  repipeFromMatch input
  do
    result <- action
    evaluate (rnf result)
    return result
    `finally` do
      savedFdH <- fdToHandle savedFd
      hDuplicateTo savedFdH stdin
      hSetBuffering stdin savedBuffering
      hClose savedFdH

repipeFromMatch :: String -> IO ()
repipeFromMatch input = do
  syncToInput input
  rest <- getContents2
  (readFd, writeFd) <- System.Posix.IO.createPipe
  _ <- forkIO $ do
    let writeAndClose = fdWrite writeFd (input ++ rest) >> closeFd writeFd
    writeAndClose `catch` \e ->
      if isResourceVanishedError e then closeFd writeFd else throwIO e
  dupTo readFd stdInput
  closeFd readFd

syncToInput :: String -> IO ()
syncToInput "" = return ()
syncToInput input = go ""
  where
    n = length input
    go buf = do
      c <- getChar
      let buf' = buf ++ [c]
      let buf'' = if length buf' > n then drop (length buf' - n) buf' else buf'
      if buf'' == input then return () else go buf''
