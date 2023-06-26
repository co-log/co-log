{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main (main) where

import Prelude hiding (log)
import Data.Text (Text,append)
import Colog ( LogAction, SimpleMsg , usingLoggerT,
              WithLog, cmap, logText,fmtSimpleMessage,formatWith,
              logTextStderr,logTextStdout
              )


example1 :: WithLog env SimpleMsg m => m ()
example1 = do
    logText "this is a demo log for simple message!"

example2 :: WithLog env SimpleMsg m => m ()
example2 = do
    logText "you see the demo log for simple message again!\n"
    

logStdoutAction :: LogAction IO SimpleMsg
logStdoutAction = cmap fmtSimpleMessage logTextStdout

logStdErrAction :: LogAction IO SimpleMsg
logStdErrAction = formatWith fmtSimpleMessage logTextStderr

selfDefinedFmtSimpleMessage :: SimpleMsg -> Text
selfDefinedFmtSimpleMessage = append "+ self defined behavior: " . fmtSimpleMessage

logByOwnFormatterAction :: LogAction IO SimpleMsg
logByOwnFormatterAction = formatWith selfDefinedFmtSimpleMessage logTextStderr

main :: IO ()
main = do
    usingLoggerT logStdoutAction example1
    usingLoggerT logStdoutAction example2
    usingLoggerT logStdErrAction example1
    usingLoggerT logStdErrAction example2
    usingLoggerT logByOwnFormatterAction example1
    usingLoggerT logByOwnFormatterAction example2
