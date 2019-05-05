# Intro: Using `LogAction`

This tutorial is an introduction to `co-log`. It contains basic examples of
using core data types and functions.

You can run this tutorial by calling the following command:

```shell
cabal new-run intro
```

## Preamble: imports and language extensions

Since this is a literate haskell file, we need to specify all our language
extensions and imports up front.

```haskell
import Colog.Core (LogAction (..), (<&), logStringStdout)
```

## Core data type

`co-log` is based on the following data type:

```idris
newtype LogAction m msg = LogAction
    { unLogAction :: msg -> m ()
    }
```

Logging action is a function from some message of user-defined type `msg` that
perform all logic inside some monad `m`. In `co-log` logging represented as a
value. With approach you can modify the way you do logging by simply performing
transformations with the value you have.

Let's first look at very basic example of using `LogAction`:

```haskell
example1 :: LogAction IO String -> IO ()
example1 logger = do
    unLogAction logger "Example 1: First message"
    unLogAction logger "Example 1: Second message"
```

If you want to do logging, you need to pass `LogAction` as an argument to your
function. In this example we used `LogAction` that takes `String`s as messages
and performs logging inside `IO` monad.

For convenience, library defines useful operator `<&` that makes logging easier:

```haskell
example2 :: LogAction IO String -> IO ()
example2 logger = do
    logger <& "Example 2: First message"
    logger <& "Example 2: Second message"
```

In order to do some logging, we need to pass some `logger` to our functions.
Here we are going to use the following `LogAction`:

```idris
logStringStdout :: LogAction IO String
```

This action just uses `putStrLn` underhood.

```haskell
main :: IO ()
main = do
    let logger = logStringStdout
    example1 logger
    example2 logger
```

And the output is exactly what you expect:

```
Example 1: First message
Example 1: Second message
Example 2: First message
Example 2: Second message
```
