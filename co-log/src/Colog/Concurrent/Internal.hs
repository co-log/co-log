{-# LANGUAGE CPP #-}

{- |
Copyright:  (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

__NOTE:__ Many thanks to Alexander Vershilov for the implementation.

This is internal module, use it on your own risk. The implementation here may be
changed without a version bump.
-}

module Colog.Concurrent.Internal
       ( BackgroundWorker (..)
       , Capacity (..)
       ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (STM, TVar)
import Numeric.Natural (Natural)

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
    , backgroundWorkerWrite    :: msg -> STM ()
      -- ^ Method for communication with the thread.
    , backgroundWorkerIsAlive  :: TVar Bool
    }
