{-# LANGUAGE FlexibleContexts #-}

{- | 'Message' with 'Severity', and logging functions for them.
-}

module Colog.Message
       ( Message (..)
       , log
       , logDebug
       , logInfo
       , logWarning
       , logError
       , fmtMessage

       , RichMessage
       , makeRich
       , fmtRichMessage
       ) where

import Control.Concurrent (ThreadId, myThreadId)
import Data.Time.Clock (UTCTime, getCurrentTime)

import Colog.Core (LogAction, Severity (..), cbind)
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
fmtMessage :: Message -> Text
fmtMessage Message{..} = "[" <> show messageSeverity <> "] " <> messageText

-- | Contains additional data to 'Message' to display more verbose information.
data RichMessage = RichMessage
    { richMessageMsg    :: {-# UNPACK #-} !Message
    , richMessageThread :: {-# UNPACK #-} !ThreadId
    , richMessageTime   :: {-# UNPACK #-} !UTCTime
    }

{- | Allows to consume 'Message' instead of 'RichMessage' by reading current
time and thread id from 'IO'.
-}
makeRich :: MonadIO m => LogAction m RichMessage -> LogAction m Message
makeRich = cbind (liftIO . toRich)
  where
    toRich :: Message -> IO RichMessage
    toRich richMessageMsg = do
        richMessageThread <- myThreadId
        richMessageTime   <- getCurrentTime
        pure RichMessage{..}

-- | Prettifies 'RichMessage' type.
fmtRichMessage :: RichMessage -> Text
fmtRichMessage RichMessage{..} =
    "[" <> show richMessageTime <> "] "
 <> "[" <> show richMessageThread <> "] "
 <> fmtMessage richMessageMsg
