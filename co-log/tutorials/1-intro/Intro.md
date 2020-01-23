# Intro: Using `LogAction`

This tutorial is an introduction to `co-log`. It contains basic examples of
using `co-log`'s core data types and functions.

You can run this tutorial by executing the following command:

```shell
cabal new-run tutorial-intro
```

## Preamble: imports and language extensions

Since this is a literate Haskell file, we need to specify all our imports up
front:

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

Logging action is a function from some message of a user-defined type `msg` that
performs all logic inside some monad `m`. In the `co-log` library, **logger** is
represented as a value. With this approach, you can modify the way you do
logging by simply performing some transformations with the value you have.

Let's first look at a very basic example of simply using `putStrLn` for logging:

```haskell
example0 :: IO ()
example0 = do
    putStrLn "Example 0: First message"
    putStrLn "Example 0: Second message"
```

Using `putStrLn` for logging is a very simple and basic approach for logging.
When your application grows bigger and more complex, you might want to introduce
some logging library to it. For example, you might want to do something from the
following list:

1. Specify messages with a given `Severity` so you can control the verbosity of
the output.
2. Automatically print timestamps, thread IDs, and source code lines of the log
messages.
3. Submit some statistics to a web server along each log message so later you
can have analytics provided by external parties.

Now, let's look at how you can use `LogAction` instead of `putStrLn` to achieve
the same goal. With `co-log`, you need to have a value of type `LogAction` that
defines how you are going to perform logging. So you configure your logging
settings separately and then pass and use this `LogAction` value. See the
following example for more details:

```haskell
example1 :: LogAction IO String -> IO ()
example1 logger = do
    unLogAction logger "Example 1: First message"
    unLogAction logger "Example 1: Second message"
```

> **NOTE:** this function currently does exactly the same thing as `example0`.
> However, given `LogAction` can do many different interesting things which you
> can configure later in one place and automatically get the proper behavior for
> your whole application instead of changing the code of every function.

If you want do logging with `co-log`, then one of the options (and the simplest
one) is to pass `LogAction` explicitly as an argument to your function. In the
example above, we are using `LogAction` that takes `String`s as messages and
performs logging inside the `IO` monad.

For convenience, the library defines the useful operator `<&` that makes the
code more concise and simpler:

```haskell
example2 :: LogAction IO String -> IO ()
example2 logger = do
    logger <& "Example 2: First message"
    logger <& "Example 2: Second message"
```

In order to do some logging, we need to pass a `logger` to our functions.
Here we are going to use the following `LogAction`:

```idris
logStringStdout :: LogAction IO String
```

This action uses `putStrLn` under the hood and just writes a given string to
`stdout`. In this particular case, using `LogAction` from `co-log` might seem
redundant. However, now it's much easier to replace the simple `putStrLn` with
something more complex and useful.

Putting it all together, we can now run our examples

```haskell
main :: IO ()
main = do
    let logger = logStringStdout
    example0
    example1 logger
    example2 logger
```

And the output is exactly what you'd expect:

```
Example 0: First message
Example 0: Second message
Example 1: First message
Example 1: Second message
Example 2: First message
Example 2: Second message
```
