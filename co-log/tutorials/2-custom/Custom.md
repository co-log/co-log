# Using a custom monad that stores `LogAction` inside its environment

This tutorial covers the more advanced topic of using the `co-log` library with
a custom application monad.

You can run this tutorial by executing the following command:

```shell
cabal new-run tutorial-custom
```

## Preamble: imports and language extensions

Since this is a literate Haskell file, we need to specify all our language
extensions and imports up front.

```haskell
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}

import Prelude hiding (log)

import Colog (pattern D, HasLog (..), pattern I, LogAction, Message, WithLog, log,
              richMessageAction)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
```

## Application environment

If you have a complex Haskell application, then you are likely to also have
non-trivial settings that configure your application environment. The
environment may store various parameters relevant to your application's
behavior. Interestingly, we can store a `LogAction` inside the same
environment to use it for our logging functions.

The environment for your application may look like this:

```haskell
data Env m = Env
    { envServerPort :: !Int
    , envLogAction  :: !(LogAction m Message)
    }
```

Several notes about this data type:

1. It stores different parameters, like the server port.
2. It stores a `LogAction` that can log `Message`s from `co-log` in the `m`
monad.
3. `Env` is parameterized by type variable `m` which is going to be the
application monad.

The next step is to define an instance of the `HasLog` typeclass for the `Env`
data type. This instance will specify how to get and update the `LogAction`
stored inside the environment.

```haskell
instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newLogAction env = env { envLogAction = newLogAction }
    {-# INLINE setLogAction #-}
```

That's it! `co-log` requires very little boilerplate.

## Application monad

Now let's define our application monad:

```haskell
newtype App a = App
    { unApp :: ReaderT (Env App) IO a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env App))
```

This monad stores `Env` parameterized by the monad itself in its context.
Nothing special is required here to tell the monad how to use the logger.

## Example

`co-log` relies on the tagless final technique for writing functions. So you
define your monadic actions with the `WithLog` constraint that allows you to
perform logging:

```haskell
example :: WithLog env Message m => m ()
example = do
    log D "First message..."
    log I "Second message..."
```

The constraint `WithLog` has three type parameters: the application environment,
the type of the message and the monad. Function `log` takes two parameters:
logger severity and the log message's text.

## Running example

Now we are ready to execute this action.

First, let's create an example environment:

```haskell
simpleEnv :: Env App
simpleEnv = Env
    { envServerPort = 8081
    , envLogAction  = richMessageAction
    }
```

Then we need to define a function that performs actions of type `App`:

```haskell
runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env
```

Putting it all together, we can specialize the `WithLog` constraint to our
`App` monad and run our example.

```haskell
main :: IO ()
main = runApp simpleEnv example
```

And the output will look like this:

![co-log output example](https://user-images.githubusercontent.com/8126674/71579355-1b804780-2af4-11ea-97eb-3f220011fd8d.png)
