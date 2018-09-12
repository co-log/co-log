{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Control.Concurrent (threadDelay)

import Colog (pattern D, LogAction, Message (..), WithLog, cmap, fmtMessage, fmtRichMessage, log,
              logInfo, logMsg, logTextStderr, logTextStdout, logWarning, makeRich, usingLoggerT,
              withLog, withLogTextFile)

example :: WithLog env Message m => m ()
example = do
    log D "First message..."
    logInfo "Second message..."

app :: (WithLog env Message m, MonadIO m) => m ()
app = do
    logWarning "Starting application..."
    liftIO $ threadDelay $ 2 * 10^(6 :: Int)
    withLog (cmap addApp) example
  where
    addApp :: Message -> Message
    addApp msg = msg { messageText = "app: " <> messageText msg }

foo :: (WithLog env String m, WithLog env Int m) => m ()
foo = do
    logMsg ("String message..." :: String)
    logMsg @Int 42

main :: IO ()
main = withLogTextFile "co-log/example/example.log" $ \logTextFile -> do
    let textAction = logTextStdout <> logTextStderr <> logTextFile
    let simpleMessageAction  = cmap fmtMessage     textAction
    let richMessageAction    = cmap fmtRichMessage textAction
    let complexMessageAction = makeRich richMessageAction

    let runApp :: LogAction IO Message -> IO ()
        runApp action = usingLoggerT action app

    runApp simpleMessageAction
    runApp complexMessageAction
