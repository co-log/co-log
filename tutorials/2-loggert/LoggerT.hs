{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main (main) where

import Control.Monad.Reader
import GHC.Stack
import Prelude hiding (log)
import Data.Text (Text,append)
import Colog ( LogAction, SimpleMsg(..), usingLoggerT, LoggerT, (<&),
              WithLog, cmap, logText,fmtSimpleMessage,formatWith,
              logTextStderr,logTextStdout, getLogAction
              )

-- logTextExample1 asks a logger from the LoggerT monad transformer, and then writes the text into the LogAction
-- it doesn't print the stack information correctly as it requires more work to handle it
logTextExample1 :: Monad m => LoggerT SimpleMsg m ()
logTextExample1 = 
    asks getLogAction >>= \logger ->
        logger <& SimpleMsg{ 
            simpleMsgStack = callStack
            , simpleMsgText = "this is a demo log for simple message!" 
            }

-- logTextExample2 logs the text down with the respective call stack information by the logger carried by env
-- logTextExample2 is an equivalent version of LoggerT as logTextExample1 with more features so we recommend you to use it
logTextExample2 :: WithLog env SimpleMsg m => m ()
logTextExample2 = do
    logText "you see the demo log for simple message again!"

logStdoutAction :: LogAction IO SimpleMsg
logStdoutAction = cmap fmtSimpleMessage logTextStdout

selfDefinedFmtSimpleMessage :: SimpleMsg -> Text
selfDefinedFmtSimpleMessage = append "+ self defined behavior: " . fmtSimpleMessage

logByOwnFormatterAction :: LogAction IO SimpleMsg
logByOwnFormatterAction = formatWith selfDefinedFmtSimpleMessage logTextStderr

main :: IO ()
main = do
    usingLoggerT logStdoutAction logTextExample1
    usingLoggerT logStdoutAction logTextExample2
    usingLoggerT logByOwnFormatterAction logTextExample1
