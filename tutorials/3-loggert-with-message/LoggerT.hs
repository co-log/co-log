{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Colog (
    LogAction,
    Message,
    Msg (..),
    WithLog,
    cmap,
    fmtMessage,
    formatWith,
    logError,
    logInfo,
    logTextStderr,
    logTextStdout,
    msgSeverity,
    msgText,
    richMessageAction,
    showSeverity,
    usingLoggerT,
 )

import Data.Text (Text)
import Prelude hiding (log)

example1 :: WithLog env Message m => m ()
example1 = do
    logInfo "this is a demo log for message!"

example2 :: WithLog env Message m => m ()
example2 = do
    logError "Mayday! Mayday! Here is an example of error log\n"

logStdoutAction :: LogAction IO Message
logStdoutAction = cmap fmtMessage logTextStdout

fmtMessageWithoutSourceLoc :: Message -> Text
fmtMessageWithoutSourceLoc Msg{..} =
    showSeverity msgSeverity
        <> msgText

-- logStdErrActionWithoutStackAction helps to log the message text and severity to the stderr
logStdErrActionWithoutStackAction :: LogAction IO Message
logStdErrActionWithoutStackAction = formatWith fmtMessageWithoutSourceLoc logTextStderr

main :: IO ()
main = do
    usingLoggerT logStdoutAction example1
    usingLoggerT logStdoutAction example2
    usingLoggerT logStdErrActionWithoutStackAction example1
    usingLoggerT logStdErrActionWithoutStackAction example2
    usingLoggerT richMessageAction example1
    usingLoggerT richMessageAction example2