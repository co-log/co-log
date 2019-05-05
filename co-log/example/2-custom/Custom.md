# Using custom monad that stores `LogAction` inside its environment

This tutorial covers more advanced topic of using `co-log` library with custom
application monad.

You can run this tutorial by calling the following command:

```shell
cabal new-run tutorial-custom
```

## Preamble: imports and language extensions

Since this is a literate haskell file, we need to specify all our language
extensions and imports up front.

```haskell
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

If you have complex Haskell application, then most likely you also have
non-trivial settings that configure your application environment. The
environment may store various parameters important for work your application.
Interestingly, we can store `LogAction` inside the same environment to use it
automatically for our logging functions.

The environment for your application can look like this:

```haskell
data Env m = Env
    { envServerPort :: !Int
    , envLogAction  :: !(LogAction m Message)
    }
```

Several notes about this data type:

1. It stores different parameters, like server port.
2. It stores `LogAction` that can log `Message` data type from `co-log` in the
   `m` monad.
3. `Env` is parameterized by type variable `m` which is going to be application
   monad.

Next step is to define an instance of the `HasLog` typeclass for the `Env` data
type. This instance will tell how to get and update `LogAction` stored inside
the environment.

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

Now let's define our application monad.

```haskell
newtype App a = App
    { unApp :: ReaderT (Env App) IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Env App))
```

This monad stores `Env` parameterized by the monad itself in it's context.
Nothing special required here to tell the monad how to use logger.

## Example

`co-log` relies on tagless final technique for writing function. So you define
your monadic actions with the `WithLog` constraint that allows you to perform
logging:

```haskell
example :: WithLog env Message m => m ()
example = do
    log D "First message..."
    log I "Second message..."
```

Constraint `WithLog` has three type parameters: application environment, type of
the message and monad. Function `log` takes two parameters: logger severity and
message text.

## Running example

Now we are ready to execute this action.

First, let's create example environment:

```haskell
simpleEnv :: Env App
simpleEnv = Env
    { envServerPort = 8081
    , envLogAction  = richMessageAction
    }
```

Then we need to define function that performs actions of type `App`:

```haskell
runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env
```

Putting all together, we can specialize `WithLog` constraint to our `App` monad
and run our example.

```haskell
main :: IO ()
main = runApp simpleEnv example
```

And the output will look like this:

![Output example](https://user-images.githubusercontent.com/4276606/57191747-79579200-6f5b-11e9-92ed-e7b728a2b9a3.png)
