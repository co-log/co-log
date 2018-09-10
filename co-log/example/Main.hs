{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Colog (LogMessage (..), WithLog, cmap, fmtLogMessage, logDebug, logInfo, logMsg,
              logTextStderr, logTextStdout, logWarning, usingLoggerT, withLog, withLogTextFile)

example :: WithLog env LogMessage m => m ()
example = do
    logDebug "First message..."
    logInfo "Second message..."

app :: WithLog env LogMessage m => m ()
app = do
    logWarning "Starting application..."
    withLog (cmap addApp) example
  where
    addApp :: LogMessage -> LogMessage
    addApp (LogMessage msg sev) = LogMessage ("app: " <> msg) sev

foo :: (WithLog env String m, WithLog env Int m) => m ()
foo = do
    logMsg ("String message..." :: String)
    logMsg @Int 42

main :: IO ()
main = withLogTextFile "co-log/example/example.log" $ \logTextFile ->
    usingLoggerT (cmap fmtLogMessage $ logTextStdout <> logTextStderr <> logTextFile) app
