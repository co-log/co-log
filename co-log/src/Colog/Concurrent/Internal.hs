{-# LANGUAGE CPP #-}
-- |
--
-- This is internal module, use it on your own risk.
-- The implementation here may be changed without a
-- version bump.
module Colog.Concurrent.Internal
       ( BackgroundWorker(..)
       , Capacity(..)
       ) where

import Control.Concurrent

-- | A wrapper type that carries capacity. The internal
-- type may be differrent for the different GHC versions.
#if MIN_VERSION_stm(2,5,0)
newtype Capacity = Capacity Natural
#else
newtype Capacity = Capacity Int
#endif

-- | Wrapper for the background thread that may
-- receive messages to process.
data BackgroundWorker msg = BackgroundWorker
  { backgroundWorkerThreadId :: !ThreadId
    -- ^ Background 'ThreadId'.
  , backgroundWorkerWrite :: msg -> STM ()
    -- ^ Method for communication with the thread.
  , backgroundWorkerIsAlive :: TVar Bool
  }
