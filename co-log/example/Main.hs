{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Control.Concurrent (threadDelay)

import Colog (pattern D, LogAction, Message (..), WithLog, cbind, cmap, defaultMessageMap,
              fmtMessage, fmtRichMessageDefault, log, logInfo, logMsg, logTextStderr, logTextStdout,
              logWarning, upgradeMessageAction, usingLoggerT, withLog, withLogTextFile)

import qualified Data.TypeRepMap as TM

example :: WithLog env Message m => m ()
example = do
    log D "First message..."
    logInfo "Second message..."

app :: (WithLog env Message m, MonadIO m) => m ()
app = do
    logWarning "Starting application..."
    liftIO $ threadDelay $ 10^(6 :: Int)
    withLog (cmap addApp) $ do
        example
        logInfo "Application finished..."
  where
    addApp :: Message -> Message
    addApp msg = msg { messageText = "app: " <> messageText msg }

foo :: (WithLog env String m, WithLog env Int m) => m ()
foo = do
    logMsg ("String message..." :: String)
    logMsg @Int 42

main :: IO ()
main = withLogTextFile "co-log/example/example.log" $ \logTextFile -> do
    let runApp :: LogAction IO Message -> IO ()
        runApp action = usingLoggerT action app

    let textAction = logTextStdout <> logTextStderr <> logTextFile

    let simpleMessageAction = cmap  fmtMessage            textAction
    let richMessageAction   = cbind fmtRichMessageDefault textAction

    let fullMessageAction = upgradeMessageAction defaultMessageMap richMessageAction
    let semiMessageAction = upgradeMessageAction
                                (TM.delete @"thread-id" defaultMessageMap)
                                richMessageAction

    runApp simpleMessageAction
    runApp fullMessageAction
    runApp semiMessageAction
