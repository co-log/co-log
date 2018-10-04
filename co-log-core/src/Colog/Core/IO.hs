{- | Implements functions related to integration with IO
-}

module Colog.Core.IO
       ( liftLogIO
       ) where


import Control.Monad.IO.Class (MonadIO, liftIO)

import Colog.Core.Action (LogAction (..))

-- | Lifts a LogAction over IO into a more general Monad
liftLogIO :: MonadIO m => LogAction IO msg -> LogAction m msg
liftLogIO (LogAction action) = LogAction (liftIO . action)