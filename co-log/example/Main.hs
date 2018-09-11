{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Colog (pattern D, Message (..), WithLog, cmap, fmtLogMessage, log, logInfo, logMsg,
              logTextStderr, logTextStdout, logWarning, usingLoggerT, withLog, withLogTextFile)

example :: WithLog env Message m => m ()
example = do
    log D "First message..."
    logInfo "Second message..."

app :: WithLog env Message m => m ()
app = do
    logWarning "Starting application..."
    withLog (cmap addApp) example
  where
    addApp :: Message -> Message
    addApp msg = msg { messageText = "app: " <> messageText msg }

foo :: (WithLog env String m, WithLog env Int m) => m ()
foo = do
    logMsg ("String message..." :: String)
    logMsg @Int 42

main :: IO ()
main = withLogTextFile "co-log/example/example.log" $ \logTextFile ->
    usingLoggerT (cmap fmtLogMessage $ logTextStdout <> logTextStderr <> logTextFile) app
