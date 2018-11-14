{- | Pure implementation of logging action.
-}

module Colog.Pure
       ( PureLoggerT (..)
       , runPureLogT

       , PureLogger
       , runPureLog

       , logMessagePure
       ) where

import Control.Monad.State (MonadState, StateT (..), modify')
import Control.Monad.Trans.Class (MonadTrans)
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.Sequence (Seq, (|>))

import Colog.Core.Action (LogAction (..))


{- | Pure monad transformer for logging. Can log any @msg@ messages. Allows to
log messages by storing them in the internal state.
-}
newtype PureLoggerT msg m a = PureLoggerT
    { runPureLoggerT :: StateT (Seq msg) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadState (Seq msg))

-- | Returns result value of 'PureLoggerT' and list of logged messages.
runPureLogT :: Functor m => PureLoggerT msg m a -> m (a, [msg])
runPureLogT = fmap (second toList) . flip runStateT mempty . runPureLoggerT

-- | 'PureLoggerT' specialized to 'Identity'
type PureLogger msg = PureLoggerT msg Identity

-- | Returns result value of 'PureLogger' and list of logged messages.
runPureLog :: PureLogger msg a -> (a, [msg])
runPureLog = runIdentity . runPureLogT

-- | 'LogAction' that prints @msg@ by appending it to the end of the sequence.
logMessagePure :: Monad m => LogAction (PureLoggerT msg m) msg
logMessagePure = LogAction $ \msg -> modify' (|> msg)
