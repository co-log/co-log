{-# LANGUAGE InstanceSigs #-}

{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Core of the @mtl@ implementation.
-}

module Colog.Monad
       ( LoggerT (..)
       , WithLog
       , logMsg
       , logMsgs
       , withLog
       , liftLogAction
       , usingLoggerT
       ) where

import Prelude hiding (log)

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Foldable (traverse_)
import GHC.Stack (HasCallStack)

import Colog.Core (HasLog (..), LogAction (..), overLogAction)

{- | @newtype@ wrapper 'ReaderT' that keeps 'LogAction' in its context.
-}
newtype LoggerT msg m a = LoggerT
    { runLoggerT :: ReaderT (LogAction (LoggerT msg m) msg) m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader (LogAction (LoggerT msg m) msg))

instance MonadTrans (LoggerT msg) where
    lift :: Monad m => m a -> LoggerT msg m a
    lift = LoggerT . lift
    {-# INLINE lift #-}

type WithLog env msg m = (MonadReader env m, HasLog env msg m, HasCallStack)

{- | Perform logging action with given message @msg@. This function works for
monads that have access to 'LogAction'.

You can use this function like this:

@
example :: 'WithLog' env 'String' m => m ()
example = __do__
    'logMsg' "First message..."
    'logMsg' "Second message..."
@
-}
logMsg :: forall msg env m . WithLog env msg m => msg -> m ()
logMsg msg = do
    LogAction log <- asks getLogAction
    log msg
{-# INLINE logMsg #-}

-- | Logs multiple messages.
logMsgs :: forall msg env f m . (Foldable f, WithLog env msg m) => f msg -> m ()
logMsgs = traverse_ logMsg
{-# INLINE logMsgs #-}

{- | Performs given monadic logging action by applying function to every logging record.

@
app :: 'WithLog' env 'String' m => m ()
app = 'withLog' ('cmap' ("app:" ++)) $ __do__
    'logMsg' "First message..."
    'logMsg' "Second message..."
@
-}
withLog :: WithLog env msg m => (LogAction m msg -> LogAction m msg) -> m a -> m a
withLog = local . overLogAction
{-# INLINE withLog #-}

liftLogAction :: (Monad m, MonadTrans t) => LogAction m msg -> LogAction (t m) msg
liftLogAction (LogAction action) = LogAction (lift . action)
{-# INLINE liftLogAction #-}

{- | Runner for 'LoggerT' monad. Let's consider one simple example of monadic
action you have:

@
app :: 'WithLog' env 'String' m => m ()
app = __do__
    'logMsg' "Starting application..."
    'withLog' ('cmap' ("app:" ++)) $ __do__
        'logMsg' "Application started."
        'logMsg' "Application finished."
@

You can use the following way of running such example:

@
'usingLoggerT' ('LogAction' 'putStrLn') app
@

And you will see this output:

@
Starting application...
app:Application started.
app:Application finished.
@
-}
usingLoggerT :: Monad m => LogAction m msg -> LoggerT msg m a -> m a
usingLoggerT action = flip runReaderT (liftLogAction action) . runLoggerT
