{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Data.Time.Clock
import Colog
import Control.Monad
import Control.Exception
import Data.Foldable
import Data.Semigroup ((<>))
import System.Environment
import System.IO
import System.Process.Typed
import System.Posix.Process
import qualified Data.Text.Encoding
import GHC.Stack
import Text.Printf
import Prelude


-- | List of benchmarks.
benchs :: [(String, IO ())]
benchs =
  [("baseline", run mempty ["message"::String])
  ,("stdout<string",
     let la = logStringStdout 
     in run la ["message"])
  ,("stdout<text",
     let la = logTextStdout
     in run la ["message"])
  ,("stdout<bytestring",
     let la = logByteStringStdout 
     in run la ["message"])
  ,("stderr<bytestring",
     let la = logByteStringStderr 
     in run la ["message"])
  ,("{stdout<>stderr}<bytestring",
     let la = logByteStringStdout <> logByteStringStderr
     in run la ["message"])
  ,("stdout<format<message",
     let la = cmap fmtMessage logTextStdout
     in run la [Message D emptyCallStack "message"])
  ,("stdout<format<message{with callstack}",
     let la = cmap fmtMessage logTextStdout
     in run la [Message D callStack "message"])
  ,("stdout<format<message{with callstack:5}",
     let la = cmap fmtMessage logTextStdout
     in nest 5 $ run la [Message D callStack "message"])
  ,("stdout<format<message{with callstack:50}",
     let la = cmap fmtMessage logTextStdout
     in nest 50 $ run la [Message D callStack "message"])
  ,("stdout<bytestring<format<message",
     let la = fmtMessage
            `cmap` (Data.Text.Encoding.encodeUtf8
                    `cmap` logByteStringStdout)
     in run la [Message D emptyCallStack "message"])
  ,("stdout<bytestring<rich-message<upgrade<message",
     let la = 
            (Data.Text.Encoding.encodeUtf8
                    `cmap` logByteStringStdout)
         richMessageAction  = cmapM fmtRichMessageDefault la
         semiMessageAction = upgradeMessageAction
                                defaultFieldMap
                                richMessageAction
     in run semiMessageAction [Message D emptyCallStack "message"])
  ]
  where
    run :: LogAction IO a -> [a] -> IO ()
    run la msgs = do
      replicateM_ 10000 $ traverse_ (unLogAction la) msgs
    nest :: HasCallStack => Int -> IO a -> IO a
    nest 0 f = f
    nest n f = (nest (n-1) f) `onException` (pure ()) -- force nesting

main :: IO ()
main = getArgs >>= \case
  [] -> do
     putStrLn "Dump 10k messages (in a forked process):"
     for_ benchs $ \(name, _) -> do
       t <- timeProcess name
       printf "%-50s %s\n" name (show t)
  (name:_) -> case name `lookup` benchs of
     Nothing -> pure ()
     Just f  -> f

-- | Measure the running time of the process.
-- The process allowed to dump data to stdout or /dev/null.
-- We measure the total running time of the process, and
-- do not verify that all logs were actually dumped, process
-- should do that on it's own.
timeProcess :: String -> IO NominalDiffTime
timeProcess n = do
  t <- getCurrentTime
  pid <- getProcessID 
  withFile "/dev/null" AppendMode $ \fnull1 -> do
    withFile "/dev/null" AppendMode $ \fnull2 -> do
      let cfg = setStdin  closed
              $ setStdout (useHandleClose fnull1)
              $ setStderr (useHandleClose fnull2)
              $ proc ("/proc/" ++ show pid ++ "/exe") [n]
      runProcess_ cfg
  t' <- getCurrentTime
  pure $ t' `diffUTCTime` t
