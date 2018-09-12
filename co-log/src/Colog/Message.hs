{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

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
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Stack (SrcLoc (..))
import System.Console.ANSI (Color (..), ColorIntensity (Vivid), ConsoleLayer (Foreground), SGR (..),
                            setSGRCode)

import Colog.Core (LogAction, Severity (..), cbind)
import Colog.Monad (WithLog, logMsg)

-- | Consist of the message 'Severity' level and the message itself.
data Message = Message
    { messageSeverity :: !Severity
    , messageStack    :: !CallStack
    , messageText     :: !Text
    }

-- | Logs the message with given 'Severity'.
log :: WithLog env Message m => Severity -> Text -> m ()
log messageSeverity messageText = logMsg Message{ messageStack = callStack, .. }

-- | Logs the message with 'Debug' severity.
logDebug :: WithLog env Message m => Text -> m ()
logDebug = withFrozenCallStack (log Debug)

-- | Logs the message with 'Info' severity.
logInfo :: WithLog env Message m => Text -> m ()
logInfo = withFrozenCallStack (log Info)

-- | Logs the message with 'Warning' severity.
logWarning :: WithLog env Message m => Text -> m ()
logWarning = withFrozenCallStack (log Warning)

-- | Logs the message with 'Error' severity.
logError :: WithLog env Message m => Text -> m ()
logError = withFrozenCallStack (log Error)

-- | Prettifies 'Message' type.
fmtMessage :: Message -> Text
fmtMessage Message{..} =
    showSeverity messageSeverity
 <> showSourceLoc messageStack
 <> messageText

-- | Prints severity in different colours
showSeverity :: Severity -> Text
showSeverity = \case
    Debug   -> color Green  "[Debug]   "
    Info    -> color Blue   "[Info]    "
    Warning -> color Yellow "[Warning] "
    Error   -> color Red    "[Error]   "
 where
    color :: Color -> Text -> Text
    color c txt = toText (setSGRCode [SetColor Foreground Vivid c])
        <> txt
        <> toText (setSGRCode [Reset])

showSourceLoc :: CallStack -> Text
showSourceLoc cs = "[" <> showCallStack <> "] "
  where
    showCallStack :: Text
    showCallStack = case drop 1 $ getCallStack cs of
        []                             -> "<unknown loc>"
        (functionName, SrcLoc{..}) : _ ->
            toText srcLocModule <> "." <> toText functionName <> "#" <> show srcLocStartLine

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
    showSeverity (messageSeverity richMessageMsg)
 <> "[" <> showTime richMessageTime <> "] "
 <> showSourceLoc (messageStack richMessageMsg)
 <> "[" <> show richMessageThread <> "] "
 <> messageText richMessageMsg
   where
     showTime :: UTCTime -> Text
     showTime t = toText
         ( formatTime defaultTimeLocale "%H:%M:%S." t
        ++ take 3 (formatTime defaultTimeLocale "%q" t)
        ++ formatTime defaultTimeLocale " %e %b %Y %Z" t
         )
