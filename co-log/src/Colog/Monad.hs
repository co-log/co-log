{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs    #-}

module Colog.Monad
       ( LoggerT (..)
       , WithLog
       , logMsg
       , withLog
       , liftLogAction
       , usingLoggerT
       ) where

import Control.Monad.Reader (MonadReader (..), ReaderT)
import Control.Monad.Trans.Class (MonadTrans (..))

import Colog.Core (HasLog (..), LogAction (..), overLogAction)

{- |
-}
newtype LoggerT msg m a = LoggerT
    { runLoggerT :: ReaderT (LogAction (LoggerT msg m) msg) m a
    } deriving (Functor, Applicative, Monad, MonadReader (LogAction (LoggerT msg m) msg))

instance MonadTrans (LoggerT msg) where
    lift :: Monad m => m a -> LoggerT msg m a
    lift = LoggerT . lift

type WithLog env msg m = (MonadReader env m, HasLog env msg m)

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
logMsg :: WithLog env msg m => msg -> m ()
logMsg msg = do
    LogAction log <- asks getLogAction
    log msg

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

liftLogAction :: (Monad m, MonadTrans t) => LogAction m msg -> LogAction (t m) msg
liftLogAction (LogAction action) = LogAction (lift . action)

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
usingLoggerT action =  flip runReaderT (liftLogAction action) . runLoggerT
