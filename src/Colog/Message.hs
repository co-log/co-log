{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeFamilies          #-}

{- |
Copyright:  (c) 2018-2022 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

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

import Prelude hiding (log)

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (Exception, displayException)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.TypeRepMap (TypeRepMap)
import GHC.Exts (IsList (..))
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Stack (CallStack, SrcLoc (..), callStack, getCallStack, withFrozenCallStack)
import GHC.TypeLits (KnownSymbol, Symbol)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid), ConsoleLayer (Foreground), SGR (..),
                            setSGRCode)

import Colog.Core (LogAction, Severity (..), cmap)
import Colog.Monad (WithLog, logMsg)

import qualified Chronos as C
import qualified Chronos.Locale.English as C
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.TypeRepMap as TM
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
type instance FieldType "threadId"  = ThreadId
type instance FieldType "posixTime" = C.Time

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

instance (KnownSymbol fieldName, a ~ m (FieldType fieldName))
      => IsLabel fieldName (a -> TM.WrapTypeable (MessageField m)) where
#if MIN_VERSION_base(4,11,0)
    fromLabel field = TM.WrapTypeable $ MessageField @fieldName field
#else
    fromLabel field = TM.WrapTypeable $ MessageField  @_ @fieldName field
#endif
    {-# INLINE fromLabel #-}

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
type FieldMap (m :: Type -> Type) = TypeRepMap (MessageField m)

{- | Default message map that contains actions to extract 'ThreadId' and
'C.Time'. Basically, the following mapping:

@
"threadId"  -> 'myThreadId'
"posixTime" -> 'C.now'
@
-}
defaultFieldMap :: MonadIO m => FieldMap m
defaultFieldMap = fromList
    [ #threadId  (liftIO myThreadId)
    , #posixTime (liftIO C.now)
    ]

{- | Contains additional data to 'Message' to display more verbose information.

@since 0.4.0.0
-}
data RichMsg (m :: Type -> Type) (msg :: Type) = RichMsg
    { richMsgMsg :: !msg
    , richMsgMap :: {-# UNPACK #-} !(FieldMap m)
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
    formatRichMessage :: Maybe ThreadId -> Maybe C.Time -> Message -> Text
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
    formatRichMessage :: Maybe ThreadId -> Maybe C.Time -> SimpleMsg -> Text
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
    -> (Maybe ThreadId -> Maybe C.Time -> msg -> Text)
    -> m Text
fmtRichMessageCustomDefault RichMsg{..} formatter = do
    maybeThreadId  <- extractField $ TM.lookup @"threadId"  richMsgMap
    maybePosixTime <- extractField $ TM.lookup @"posixTime" richMsgMap
    pure $ formatter maybeThreadId maybePosixTime richMsgMsg

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
