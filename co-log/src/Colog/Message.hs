{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

{- | 'Message' with 'Severity', and logging functions for them.
-}

module Colog.Message
       ( Message (..)
       , log
       , logDebug
       , logInfo
       , logWarning
       , logError

       , fmtLogMessage
       ) where

import Colog.Core.Severity (Severity (..))
import Colog.Monad (WithLog, logMsg)

-- | Consist of the message 'Severity' level and the message itself.
data Message = Message
    { messageSeverity ::                !Severity
    , messageText     :: {-# UNPACK #-} !Text
    }

-- | Logs the message with given 'Severity'.
log :: WithLog env Message m => Severity -> Text -> m ()
log messageSeverity messageText = logMsg Message{..}

-- | Logs the message with 'Debug' severity.
logDebug :: WithLog env Message m => Text -> m ()
logDebug = log Debug

-- | Logs the message with 'Info' severity.
logInfo :: WithLog env Message m => Text -> m ()
logInfo = log Info

-- | Logs the message with 'Warning' severity.
logWarning :: WithLog env Message m => Text -> m ()
logWarning = log Warning

-- | Logs the message with 'Error' severity.
logError :: WithLog env Message m => Text -> m ()
logError = log Error

-- | Prettifies 'Message' type.
fmtLogMessage :: Message -> Text
fmtLogMessage Message{..} = "[" <> show messageSeverity <> "] " <> messageText
