# co-log

![Co-logo](https://user-images.githubusercontent.com/8126674/80955687-92f21a80-8df7-11ea-90d3-422dafdc8391.png)

[![GitHub CI](https://github.com/kowainik/co-log/workflows/CI/badge.svg)](https://github.com/kowainik/co-log/actions)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/kowainik/co-log/blob/main/LICENSE)

`co-log` is a composable and configurable logging framework. It
combines all the benefits of Haskell idioms to provide a reasonable
and convenient interface. Although the library design uses some advanced
concepts in its core, we are striving to provide beginner-friendly API. The
library also provides the complete documentation with a lot of beginner-friendly
examples, explanations and tutorials to guide users. The combination of a
pragmatic approach to logging and fundamental Haskell abstractions allows us to
create a highly composable and configurable logging framework.

If you're interested in how different Haskell typeclasses are used to
implement core functions of `co-log`, you can read the following blog
post which goes into detail about the internal implementation specifics:

* [co-log: Composable Contravariant Combinatorial Comonadic Configurable Convenient Logging](https://kowainik.github.io/posts/2018-09-25-co-log)

## Co-Log Family

Co-Log is a family of repositories for a composable and configurable logging
framework `co-log`.

Here is the list of currently available repositories and libraries that you can
check out:

|                                                                   |                                                                                                                                                        |                                    |
| :---------------------------------------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------- | :--------------------------------- |
| [`co-log-core`](https://github.com/co-log/co-log-core)            | lightweight package with basic data types and general idea which depends only on `base`                                                                | [![Hackage][hk-img-core]][hk-core] |
| [`co-log`](https://github.com/co-log/co-log)                      | taggless final implementation of logging library based on `co-log-core`                                                                                | [![Hackage][hk-img]][hk]           |
| [`co-log-polysemy`](https://github.com/co-log/co-log-polysemy)    | implementation of logging library based on `co-log-core` and the [`polysemy`](http://hackage.haskell.org/package/polysemy) extensible effects library. | [![Hackage][hk-img-ps]][hk-ps]     |
| [`co-log-benchmarks`](https://github.com/co-log/co-log-benchmarks) | benchmarks of the `co-log` library                                                                                                                     | -

## `co-log` library

Logging library based on [`co-log-core`](https://github.com/co-log/co-log-core)
package. Provides ready-to-go implementation of logging. This README contains
_How to_ tutorial on using this library. This tutorial explains step by step how
to integrate `co-log` into small basic project, specifically how to replace
`putStrLn` used for logging with library provided logging.

All code below can be compiled and run with the following commands:

```shell
$ cabal build
$ cabal exec readme
```

## Preamble: imports and language extensions

Since this is a literate haskell file, we need to specify all our language
extensions and imports up front.

```haskell
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (MonadIO, liftIO)

import Colog (Message, WithLog, cmap, fmtMessage, logDebug, logInfo, logTextStdout, logWarning,
              usingLoggerT)

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
```

## Simple IO function example

Consider the following function that reads lines from `stdin` and outputs
different feedback depending on the line size.

```haskell
processLinesBasic :: IO ()
processLinesBasic = do
    line <- TextIO.getLine
    case Text.length line of
        0 -> do
            -- here goes logging
            TextIO.putStrLn ">>>> Empty input"
            processLinesBasic
        n -> do
            TextIO.putStrLn ">>>> Correct input"
            TextIO.putStrLn $ "Line length: " <> Text.pack (show n)
```

This code mixes application logic with logging of the steps. It's convenient to
have logging to observe behavior of the application. But `putStrLn` is very
simple and primitive way to log things.

## Using `co-log` library

In order to use `co-log` library, we need to refactor `processLinesBasic`
function in the following way:

```haskell
processLinesLog :: (WithLog env Message m, MonadIO m) => m ()
processLinesLog = do
    line <- liftIO TextIO.getLine
    case Text.length line of
        0 -> do
            -- here goes logging
            logWarning "Empty input"
            processLinesLog
        n -> do
            logDebug "Correct line"
            logInfo $ "Line length: " <> Text.pack (show n)
```

Let's summarize required changes:

1. Make type more polymorphic: `(WithLog env Message m, MonadIO m) => m ()`
2. Add `liftIO` to all `IO` functions.
3. Replace `putStrLn` with proper `log*` function.

## Running actions

Let's run both functions:

```haskell
main :: IO ()
main = do
    processLinesBasic

    let action = cmap fmtMessage logTextStdout
    usingLoggerT action processLinesLog
```

And here is how output looks like:

![screenshot from 2018-09-17 20-52-01](https://user-images.githubusercontent.com/4276606/45623973-8bafb900-babb-11e8-9e20-4369a5a8e5ff.png)

## More Tutorials

To provide a more user-friendly introduction to the library, we've
created the tutorial series which introduces the main concepts behind `co-log`
smoothly:

* [Intro: Using `LogAction`](https://github.com/co-log/co-log/blob/main/tutorials/1-intro/Intro.md)
* [Using custom monad that stores `LogAction` inside its environment](https://github.com/co-log/co-log/blob/main/tutorials/2-custom/Custom.md)

`co-log` also cares about concurrent logging. For this purpose we have the `concurrent-playground`
executable where we experiment with different multithreading scenarios to test the library's behavior.
You can find it here:

* [tutorials/Concurrent.hs](tutorials/Concurrent.hs)

[hk-img]: https://img.shields.io/hackage/v/co-log.svg?logo=haskell
[hk-img-ps]: https://img.shields.io/hackage/v/co-log-polysemy.svg?logo=haskell
[hk-img-core]: https://img.shields.io/hackage/v/co-log-core.svg?logo=haskell
[hk]: https://hackage.haskell.org/package/co-log
[hk-ps]: https://hackage.haskell.org/package/co-log-polysemy
[hk-core]: https://hackage.haskell.org/package/co-log-core
