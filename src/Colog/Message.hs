{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Copyright:  (c) 2018-2022 Kowainik, 2023-2025 Co-Log
SPDX-License-Identifier: MPL-2.0

This module contains logging messages data types along with the formatting and
logging actions for them.
-}

module Colog.Message
       ( -- * Simple message type
         -- ** Type
         SimpleMsg (..)
         -- ** Logging
       , logText
         -- ** Formatting
       , fmtSimpleMessage
       , formatWith

         -- * Core messaging
         -- ** Types
       , Msg (..)
       , Message
         -- ** Logging
       , log
       , logDebug
       , logInfo
       , logWarning
       , logError
       , logException
         -- ** Formatting
       , fmtMessage
       , showSeverity
       , showSourceLoc
       , showTime
       , showTimeOffset
       , showThreadId

         -- * Externally extensible message type
         -- ** Formatter
       , FieldFmt
       , defaultFieldFmt
       , defaultFieldFmtSimple
       , timeFmt
       , threadFmt

         -- ** Extensible message
       , RichMessage
       , RichMsg (..)
       , fmtRichMessageDefault
       , fmtSimpleRichMessageDefault
       , upgradeMessageAction
       ) where

import Prelude hiding (lookup, log)

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (Exception, displayException)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import GHC.Stack (CallStack, SrcLoc (..), callStack, getCallStack, withFrozenCallStack)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid), ConsoleLayer (Foreground), SGR (..),
                            setSGRCode)

import Colog.Core (LogAction, Severity (..), cmap)
import Colog.Monad (WithLog, logMsg)

import qualified Chronos as C
import qualified Chronos.Locale.English as C
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Vector as Vector

----------------------------------------------------------------------------
-- Plain message
----------------------------------------------------------------------------

{- | General logging message data type. Contains the following fields:

1. Polymorphic severity. This can be anything you want if you need more
flexibility.
2. Function 'CallStack'. It provides useful information about source code
locations where each particular function was called.
3. Custom text for logging.
-}
data Msg sev = Msg
    { msgSeverity :: !sev
    , msgStack    :: !CallStack
    , msgText     ::  Text
    }

{- | Message data type without 'Severity'. Use 'logText' to log
messages of this type.

@since 0.4.0.0
-}
data SimpleMsg = SimpleMsg
    { simpleMsgStack :: !CallStack
    , simpleMsgText  :: !Text
    }

{- | 'Msg' parametrized by the 'Severity' type. Most formatting functions in
this module work with 'Severity' from @co-log-core@.
-}
type Message = Msg Severity

-- | Logs the message with given severity @sev@.
log :: WithLog env (Msg sev) m => sev -> Text -> m ()
log msgSeverity msgText =
    withFrozenCallStack (logMsg Msg{ msgStack = callStack, .. })

-- | Logs the message with the 'Debug' severity.
logDebug :: WithLog env Message m => Text -> m ()
logDebug = withFrozenCallStack (log Debug)

-- | Logs the message with the 'Info' severity.
logInfo :: WithLog env Message m => Text -> m ()
logInfo = withFrozenCallStack (log Info)

-- | Logs the message with the 'Warning' severity.
logWarning :: WithLog env Message m => Text -> m ()
logWarning = withFrozenCallStack (log Warning)

-- | Logs the message with the 'Error' severity.
logError :: WithLog env Message m => Text -> m ()
logError = withFrozenCallStack (log Error)

-- | Logs 'Exception' message with the 'Error' severity.
logException :: forall e m env . (WithLog env Message m, Exception e) => e -> m ()
logException = withFrozenCallStack (logError . T.pack . displayException)

{- | Logs 'SimpleMsg' without severity, only 'CallStack' and 'Text'
body message.

@since 0.4.0.0
-}
logText :: WithLog env SimpleMsg m => Text -> m ()
logText msgText = withFrozenCallStack (logMsg SimpleMsg{ simpleMsgStack = callStack, simpleMsgText = msgText })

{- | Formats the 'Message' type according to the following format:

@
[Severity] [SourceLocation] \<Text message\>
@

__Examples:__

@
[Warning] [Main.app#39] Starting application...
[Debug]   [Main.example#34] app: First message...
@

See 'fmtRichMessageDefault' for a richer format.
-}
fmtMessage :: Message -> Text
fmtMessage Msg{..} =
    showSeverity msgSeverity
    <> showSourceLoc msgStack
    <> msgText

{- | Formats the 'SimpleMsg' type in according to the following format:

@
[SourceLocation] \<Text message\>
@

__Examples:__

@
[Main.app#39] Starting application...
[Main.example#34] app: First message...
@

See 'fmtSimpleRichMessageDefault' for richer format.

@since 0.4.0.0
-}
fmtSimpleMessage :: SimpleMsg -> Text
fmtSimpleMessage SimpleMsg{..} = showSourceLoc simpleMsgStack <> simpleMsgText

{- | Alias for 'cmap' specialized for formatting purposes. If you have
an action that can output 'Text' (for example
'Colog.Actions.logTextStdout'), you can convert it to the action that
can print 'SimpleMsg' or 'Message':

@
logSimpleMsgStdout :: 'LogAction' 'IO' 'SimpleMsg'
logSimpleMsgStdout = 'formatWith' 'fmtSimpleMessage' 'Colog.Actions.logTextStdout'

logMessageStdout :: 'LogAction' 'IO' 'Message'
logMessageStdout = 'formatWith' 'fmtMessage' 'Colog.Actions.logTextStdout'
@

@since 0.4.0.0
-}
formatWith :: (msg -> Text) -> LogAction m Text -> LogAction m msg
formatWith = cmap
{-# INLINE formatWith #-}

{- | Formats severity in different colors with alignment.
-}
showSeverity :: Severity -> Text
showSeverity = \case
    Debug   -> color Green  "[Debug]   "
    Info    -> color Blue   "[Info]    "
    Warning -> color Yellow "[Warning] "
    Error   -> color Red    "[Error]   "
 where
    color :: Color -> Text -> Text
    color c txt =
        T.pack (setSGRCode [SetColor Foreground Vivid c])
        <> txt
        <> T.pack (setSGRCode [Reset])

square :: Text -> Text
square t = "[" <> t <> "] "

{- | Shows source code locations in the following format:

@
[Main.example#35]
@
-}
showSourceLoc :: CallStack -> Text
showSourceLoc cs = square showCallStack
  where
    showCallStack :: Text
    showCallStack = case getCallStack cs of
        []                             -> "<unknown loc>"
        [(name, loc)]                  -> showLoc name loc
        (_, loc) : (callerName, _) : _ -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc{..} =
        T.pack srcLocModule <> "." <> T.pack name <> "#" <> T.pack (show srcLocStartLine)


{- | Formatter for fields of a message.
-}
type FieldFmt m msg = RichMsg m msg -> Text -> m Text

{- | Default message field formatter that provides the following format:

@
[Severity] [Time] [SourceLocation] [ThreadId] \<Text message\>
@
-}
defaultFieldFmt :: MonadIO m => FieldFmt m Message
defaultFieldFmt msg =
  let sevFmt   = pure . (showSeverity (msgSeverity (richMsgMsg msg)) <>)
      stackFmt = pure . (showSourceLoc (msgStack (richMsgMsg msg)) <>)
  in sevFmt <=< timeFmt <=< stackFmt <=< threadFmt

{- | Default message field formatter that provides the following format:

@
[Time] [SourceLocation] [ThreadId] \<Text message\>
@
-}
defaultFieldFmtSimple :: MonadIO m => FieldFmt m SimpleMsg
defaultFieldFmtSimple msg =
  let stackFmt = pure . (showSourceLoc (simpleMsgStack (richMsgMsg msg)) <>)
  in timeFmt <=< stackFmt <=< threadFmt

{- | Convenience function to format time field. Basically appends something like:

@
[03 May 2019 05:23:19.058 +00:00]
@
-}
timeFmt :: MonadIO m => Text -> m Text
timeFmt t = (<> t) . showTime <$> liftIO C.now

{- | Convenience function to format thread field. Basically appends something like:

@
[ThreadId 11]
@
-}
threadFmt :: MonadIO m => Text -> m Text
threadFmt t = (<> t) . showThreadId <$> liftIO myThreadId

{- | Contains additional data to 'Message' to display more verbose information.
-}
data RichMsg (m :: Type -> Type) (msg :: Type) = RichMsg
    { richMsgMsg :: !msg
    , richMsgFmt :: !(RichMsg m msg -> Text -> m Text)
    }

-- | Specialized version of 'RichMsg' that stores severity, callstack and text message.
type RichMessage m = RichMsg m Message

{- | Formats 'RichMessage' in the following way:

@
[Severity] [Time] [SourceLocation] [ThreadId] \<Text message\>
@

__Examples:__

@
[Debug]   [03 May 2019 05:23:19.058 +00:00] [Main.example#34] [ThreadId 11] app: First message...
[Info]    [03 May 2019 05:23:19.059 +00:00] [Main.example#35] [ThreadId 11] app: Second message...
@

See 'fmtMessage' if you don't need both time and thread ID.
-}
fmtRichMessageDefault :: RichMessage m -> m Text
fmtRichMessageDefault msg = richMsgFmt msg msg (msgText (richMsgMsg msg))

{- | Formats 'RichMessage' in the following way:

@
[Time] [SourceLocation] [ThreadId] \<Text message\>
@

__Examples:__

@
[03 May 2019 05:23:19.058 +00:00] [Main.example#34] [ThreadId 11] app: First message...
[03 May 2019 05:23:19.059 +00:00] [Main.example#35] [ThreadId 11] app: Second message...
@

Practically, it formats a message as 'fmtRichMessageDefault' without the severity information.

@since 0.4.0.0
-}
fmtSimpleRichMessageDefault :: RichMsg m SimpleMsg -> m Text
fmtSimpleRichMessageDefault msg = richMsgFmt msg msg (simpleMsgText (richMsgMsg msg))

{- | Shows time in the following format:

>>> showTime $ C.Time 1577656800000000000
"[29 Dec 2019 22:00:00.000 +00:00] "
-}
showTime :: C.Time -> Text
showTime t =
    square
    $ toStrict
    $ TB.toLazyText
    $ builderDmyHMSz (C.timeToOffsetDatetime (C.Offset 0) t)

{- | Shows time in the following format:

>>> showTimeOffset $ C.timeToOffsetDatetime (C.Offset $ -120) $ C.Time 1577656800000000000
"[29 Dec 2019 20:00:00.000 -02:00] "
-}
showTimeOffset :: C.OffsetDatetime -> Text
showTimeOffset = square . toStrict . TB.toLazyText . builderDmyHMSz

----------------------------------------------------------------------------
-- Chronos extra
----------------------------------------------------------------------------

{- | Given a 'OffsetDatetime', constructs a 'Text' 'TB.Builder' corresponding to a
Day\/Month\/Year,Hour\/Minute\/Second\/Offset encoding of the given 'OffsetDatetime'.

Example: @29 Dec 2019 22:00:00.000 +00:00@
-}
builderDmyHMSz :: C.OffsetDatetime -> TB.Builder
builderDmyHMSz (C.OffsetDatetime (C.Datetime date time) offset) =
       builderDmy date
    <> spaceSep
    <> C.builder_HMS (C.SubsecondPrecisionFixed 3) (Just ':') time
    <> spaceSep
    <> C.builderOffset C.OffsetFormatColonOn offset
  where
    spaceSep :: TB.Builder
    spaceSep = TB.singleton ' '

    {- Given a 'Date' construct a 'Text' 'TB.Builder'
    corresponding to a Day\/Month\/Year encoding.

    Example: @01 Jan 2020@
    -}
    builderDmy :: C.Date -> TB.Builder
    builderDmy (C.Date (C.Year y) m d) =
           zeroPadDayOfMonth d
        <> spaceSep
        <> TB.fromText (C.caseMonth C.abbreviated m)
        <> spaceSep
        <> TB.decimal y


    zeroPadDayOfMonth :: C.DayOfMonth -> TB.Builder
    zeroPadDayOfMonth (C.DayOfMonth d) =
        if d < 100
        then Vector.unsafeIndex twoDigitTextBuilder d
        else TB.decimal d

    twoDigitTextBuilder :: Vector.Vector TB.Builder
    twoDigitTextBuilder = Vector.fromList $
        map (TB.fromText . T.pack) twoDigitStrings
    {-# NOINLINE twoDigitTextBuilder #-}

    twoDigitStrings :: [String]
    twoDigitStrings =
        [ "00","01","02","03","04","05","06","07","08","09"
        , "10","11","12","13","14","15","16","17","18","19"
        , "20","21","22","23","24","25","26","27","28","29"
        , "30","31","32","33","34","35","36","37","38","39"
        , "40","41","42","43","44","45","46","47","48","49"
        , "50","51","52","53","54","55","56","57","58","59"
        , "60","61","62","63","64","65","66","67","68","69"
        , "70","71","72","73","74","75","76","77","78","79"
        , "80","81","82","83","84","85","86","87","88","89"
        , "90","91","92","93","94","95","96","97","98","99"
        ]

----------------------------------------------------------------------------
-- Utility functions
----------------------------------------------------------------------------

{- | Shows a thread id in the following format:

__>>__ showThreadId <$> Control.Concurrent.myThreadId
"[ThreadId 4898] "
-}
showThreadId :: ThreadId -> Text
showThreadId = square . T.pack . show

{- | Switch to the given field formatting.
-}
upgradeMessageAction
    :: forall m msg .
       FieldFmt m msg
    -> LogAction m (RichMsg m msg)
    -> LogAction m msg
upgradeMessageAction fieldMmt = cmap addFmt
  where
    addFmt :: msg -> RichMsg m msg
    addFmt msg = RichMsg msg fieldMmt
