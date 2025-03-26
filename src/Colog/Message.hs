{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
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
         -- ** Field of the dependent map
       , FieldType
       , MessageField (..)
       , unMessageField
       , extractField
         -- ** Dependent map that allows to extend logging message
       , FieldMap
       , defaultFieldMap

         -- ** Extensible message
       , RichMessage
       , RichMsg (..)
       , fmtRichMessageDefault
       , fmtSimpleRichMessageDefault
       , fmtRichMessageCustomDefault
       , upgradeMessageAction
       ) where

import Prelude hiding (lookup, log)

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (Exception, displayException)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Dependent.Map (DMap, fromList, lookup)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Stack (CallStack, SrcLoc (..), callStack, getCallStack, withFrozenCallStack)
import GHC.TypeLits (Symbol)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid), ConsoleLayer (Foreground), SGR (..),
                            setSGRCode)
import Type.Reflection (TypeRep, typeRep)

import Colog.Core (LogAction, Severity (..), cmap)
import Colog.Monad (WithLog, logMsg)

import qualified Data.Time as C
import qualified Data.Text as T

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

{- | Formats severity in different colours with alignment.
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

----------------------------------------------------------------------------
-- Externally extensible message
----------------------------------------------------------------------------

{- | Open type family that maps some user defined tags (type names) to actual
types. The type family is open so you can add new instances.
-}
type family FieldType (fieldName :: Symbol) :: Type
type instance FieldType "threadId" = ThreadId
type instance FieldType "utcTime"  = C.UTCTime

{- | @newtype@ wrapper. Stores monadic ability to extract value of 'FieldType'.

__Implementation detail:__ this exotic writing of 'MessageField' is required in
order to use it nicer with type applications. So users can write

@
MessageField @"threadId" myThreadId
@

instead of

@
MessageField @_ @"threadId" myThreadId
@

Simpler version of this @newtype@:

@
newtype MessageField m fieldName = MessageField
    { unMesssageField :: m (FieldType fieldName)
    }
@
-}
newtype MessageField (m :: Type -> Type) (fieldName :: Symbol) where
    MessageField :: forall fieldName m . m (FieldType fieldName) -> MessageField m fieldName

-- | Extracts field from the 'MessageField' constructor.
unMessageField :: forall fieldName m . MessageField m fieldName -> m (FieldType fieldName)
unMessageField (MessageField f) = f
{-# INLINE unMessageField #-}

-- | Helper function to deal with 'MessageField' when looking it up in the 'FieldMap'.
extractField
    :: Applicative m
    => Maybe (MessageField m fieldName)
    -> m (Maybe (FieldType fieldName))
extractField = traverse unMessageField
{-# INLINE extractField #-}

-- same as:
-- extractField = \case
--    Nothing -> pure Nothing
--    Just (MessageField field) -> Just <$> field

{- | Depedent map from type level strings to the corresponding types. See
'FieldType' for mapping between names and types.
-}
type FieldMap m = DMap TypeRep (MessageField m)

{- | Default message map that contains actions to extract 'ThreadId' and
'C.UTCTime'. Basically, the following mapping:

@
"threadId" -> 'myThreadId'
"utcTime"  -> 'C.getCurrentTime'
@
-}
defaultFieldMap :: MonadIO m => FieldMap m
defaultFieldMap = fromList
    [ typeRep @"threadId" :=> MessageField (liftIO myThreadId)
    , typeRep @"utcTime"  :=> MessageField (liftIO C.getCurrentTime)
    ]

{- | Contains additional data to 'Message' to display more verbose information.

@since 0.4.0.0
-}
data RichMsg (m :: Type -> Type) (msg :: Type) = RichMsg
    { richMsgMsg :: !msg
    , richMsgMap :: !(FieldMap m)
    } deriving stock (Functor)

-- | Specialised version of 'RichMsg' that stores severity, callstack and text message.
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
fmtRichMessageDefault :: MonadIO m => RichMessage m -> m Text
fmtRichMessageDefault msg = fmtRichMessageCustomDefault msg formatRichMessage
  where
    formatRichMessage :: Maybe ThreadId -> Maybe C.UTCTime -> Message -> Text
    formatRichMessage (maybe "" showThreadId -> thread) (maybe "" showTime -> time) Msg{..} =
        showSeverity msgSeverity
     <> time
     <> showSourceLoc msgStack
     <> thread
     <> msgText

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
fmtSimpleRichMessageDefault :: MonadIO m => RichMsg m SimpleMsg -> m Text
fmtSimpleRichMessageDefault msg = fmtRichMessageCustomDefault msg formatRichMessage
  where
    formatRichMessage :: Maybe ThreadId -> Maybe C.UTCTime -> SimpleMsg -> Text
    formatRichMessage (maybe "" showThreadId -> thread) (maybe "" showTime -> time) SimpleMsg{..} =
        time
     <> showSourceLoc simpleMsgStack
     <> thread
     <> simpleMsgText
{- | Custom formatting function for 'RichMsg'. It extracts 'ThreadId'
and 'C.Time' from fields and allows you to specify how to format them.

@since 0.4.0.0
-}
fmtRichMessageCustomDefault
    :: MonadIO m
    => RichMsg m msg
    -> (Maybe ThreadId -> Maybe C.UTCTime -> msg -> Text)
    -> m Text
fmtRichMessageCustomDefault RichMsg{..} formatter = do
    maybeThreadId <- extractField $ lookup (typeRep @"threadId")  richMsgMap
    maybeUtcTime  <- extractField $ lookup (typeRep @"utcTime") richMsgMap
    pure $ formatter maybeThreadId maybeUtcTime richMsgMsg

{- | Shows time in the following format:

>>> showTime $ C.UTCTime (C.fromGregorian 2019 12 29) (C.secondsToDiffTime 3600 * 22)
"[29 Dec 2019 22:00:00.000 +00:00] "
-}
showTime :: C.UTCTime -> Text
showTime = showTimeOffset . C.utcToZonedTime C.utc

{- | Shows time in the following format:

>>> showTimeOffset $ C.utcToZonedTime (C.hoursToTimeZone (-2)) (C.UTCTime (C.fromGregorian 2019 12 29) (C.secondsToDiffTime 3600 * 22))
"[29 Dec 2019 20:00:00.000 -02:00] "
-}
showTimeOffset :: C.ZonedTime -> Text
showTimeOffset = T.pack . C.formatTime C.defaultTimeLocale "[%d %b %Y %H:%M:%S%3Q %Ez] "

----------------------------------------------------------------------------
-- Utility functions
----------------------------------------------------------------------------

{- | Shows a thread id in the following format:

__>>__ showThreadId <$> Control.Concurrent.myThreadId
"[ThreadId 4898] "
-}
showThreadId :: ThreadId -> Text
showThreadId = square . T.pack . show

{- | Allows to extend basic 'Message' type with given dependent map of fields.
-}
upgradeMessageAction
    :: forall m msg .
       FieldMap m
    -> LogAction m (RichMsg m msg)
    -> LogAction m msg
upgradeMessageAction fieldMap = cmap addMap
  where
    addMap :: msg -> RichMsg m msg
    addMap msg = RichMsg msg fieldMap
