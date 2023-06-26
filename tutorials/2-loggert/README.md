# Simple Message and LoggerT

This tutorial will show you how to use LoggerT and Simple message to log with more information in a more flexiable way.

You can run this tutorial by executing the following command:

```shell
cabal new-run tutorial-loggert-simple
```

## Preamble: imports and language extensions

Since this is a literate Haskell file, we need to specify all our language
extensions and imports up front.

```haskell
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
```

## LoggerT
The `LoggerT` monad transformer wraps a ReaderT that keeps `LogAction` in its context. It denotes a 
computation of logging. So you define your monadic actions with the `WithLog` constraint that allows you to perform logging:
```haskell
example1 :: WithLog env SimpleMsg m => m ()
example1 = do
    logText "this is a demo log for simple message!"

example2 :: WithLog env SimpleMsg m => m ()
example2 = do
    logText "you see the demo log for simple message again!\n"
```
The `WithLog` constraint has three type parameters: the application environment,
the type of the message and the monad. The actions constraint by `WithLog` could be used as `LoggerT`.


## Simple Message
The simple message is data type without `severity`. It contains a callstack information and a text message.
```idris
data SimpleMsg = SimpleMsg
    { simpleMsgStack :: !CallStack
    , simpleMsgText  :: !Text
    }
```
When logging, simple messages require a format for transforming from simple messages to text. We can either use `formatWith` or its alias `cmap`
to combine it with the `LogAction`.

```haskell
logStdoutAction :: LogAction IO SimpleMsg
logStdoutAction = cmap fmtSimpleMessage logTextStdout

logStdErrAction :: LogAction IO SimpleMsg
logStdErrAction = formatWith fmtSimpleMessage logTextStderr
```
What's more, it's available to define a own formatter.
```haskell
selfDefinedFmtSimpleMessage :: SimpleMsg -> Text
selfDefinedFmtSimpleMessage = append "+ self defined behavior: " . fmtSimpleMessage

logByOwnFormatterAction :: LogAction IO SimpleMsg
logByOwnFormatterAction = formatWith selfDefinedFmtSimpleMessage logTextStderr
```

## Running example

Now we are ready to execute those actions defined above.
```haskell
main :: IO ()
main = do
    usingLoggerT logStdoutAction example1
    usingLoggerT logStdoutAction example2
    usingLoggerT logStdErrAction example1
    usingLoggerT logStdErrAction example2
    usingLoggerT logByOwnFormatterAction example1
    usingLoggerT logByOwnFormatterAction example2
```

Run command `cabal new-run tutorial-loggert-simple`.

And the output will look like this:

![](../img/2-loggert-output.jpg)
