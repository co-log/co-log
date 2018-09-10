{-# LANGUAGE FlexibleContexts #-}

module Colog.Message
       ( LogMessage (..)
       , log
       , logDebug
       , logInfo
       , logNotice
       , logWarning
       , logError

       , fmtLogMessage
       ) where

import Colog.Core.Severity (Severity (..))
import Colog.Monad (WithLog, logMsg)

data LogMessage = LogMessage Text Severity

-- | Logs the message with given 'Severity'.
log :: WithLog env LogMessage m => Severity -> Text -> m ()
log sev msg = logMsg (LogMessage msg sev)

-- | Logs the message with 'Debug' severity.
logDebug :: WithLog env LogMessage m => Text -> m ()
logDebug = log Debug

-- | Logs the message with 'Info' severity.
logInfo :: WithLog env LogMessage m => Text -> m ()
logInfo = log Info

-- | Logs the message with 'Notice' severity.
logNotice :: WithLog env LogMessage m => Text -> m ()
logNotice = log Notice

-- | Logs the message with 'Warning' severity.
logWarning :: WithLog env LogMessage m => Text -> m ()
logWarning = log Warning

-- | Logs the message with 'Error' severity.
logError :: WithLog env LogMessage m => Text -> m ()
logError = log Error

fmtLogMessage :: LogMessage -> Text
fmtLogMessage (LogMessage msg sev) = "[" <> show sev <> "] " <> msg
