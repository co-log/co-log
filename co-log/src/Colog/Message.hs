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
Copyright:  (c) 2018-2019 Kowainik
License:    MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains logging messages data types along with the formatting and
logging actions for them.
-}

module Colog.Message
       ( -- * Basic message type
         Msg (..)
       , Message
       , log
       , logDebug
       , logInfo
       , logWarning
       , logError
       , logException

         -- * Formatting functions
       , fmtMessage
       , showSeverity
       , showSourceLoc

         -- * Externally extensible message type
         -- ** Field of the dependent map
       , FieldType
       , MessageField (..)
       , unMessageField
       , extractField
         -- ** Dependent map that allows to extend logging message
       , FieldMap
       , defaultFieldMap

       , RichMessage
       , SimpleMsg(..)
       , fmtRichMessageDefault
       , fmtSimpleRichMessageDefault
       , upgradeMessageAction
       ) where

import Prelude hiding (log)

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (Exception, displayException)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Kind (Type)
import Data.Semigroup ((<>))
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
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.TypeRepMap as TM

----------------------------------------------------------------------------
-- Plain message
----------------------------------------------------------------------------

{- | General logging message data type. Contains the following fields:

1. Polymorhic severity. This can be anything you want if you need more
flexibility.
2. Function 'CallStack'. It provides useful information about source code
locations where each particular function was called.
3. Custom text for logging.
-}
data Msg sev = Msg
    { msgSeverity :: !sev
    , msgStack    :: !CallStack
    , msgText     :: !Text
    }

data SimpleMsg = SimpleMsg
    { simpleMsgStack :: !CallStack
    , simpleMsgText :: !Text
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

-- | Logs 'Exception' message.
logException :: forall e m env . (WithLog env Message m, Exception e) => e -> m ()
logException = withFrozenCallStack (logError . T.pack . displayException)

{- | Formats the 'Message' type in according to the following format:

@
[Severity] [SourceLocation] \<Text message\>
@

__Examples:__

@
[Warning] [Main.app#39] Starting application...
[Debug]   [Main.example#34] app: First message...
@

See 'fmtRichMessageDefault' for richer format.
-}
fmtMessage :: Message -> Text
fmtMessage Msg{..} =
    showSeverity msgSeverity
    <> showSourceLoc msgStack
    <> msgText

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

{- | Show source code locations in the following format:

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
type instance FieldType "posixTime"  = C.Time

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

-- | Contains additional data to 'Message' to display more verbose information.
data RichMsg (m :: Type -> Type) (msg :: Type) = RichMessage
    { richMsgMsg :: !msg
    , richMsgMap :: {-# UNPACK #-} !(FieldMap m)
    } deriving (Functor)

-- | Specialised version of 'RichMsg' that stores severity, callstack and text message.
type RichMessage m = RichMsg m Message

{- | Formats 'RichMessage' in the following way:

@
[Severity] [Time] [SourceLocation] [ThreadId] \<Text message\>
@

__Examples:__

@
[Debug]   [03 05 2019 05:23:19.058] [Main.example#34] [ThreadId 11] app: First message...
[Info]    [03 05 2019 05:23:19.059] [Main.example#35] [ThreadId 11] app: Second message...
@

See 'fmtMessage' if you don't need both time and thread id.
-}
fmtRichMessageDefault :: MonadIO m => RichMessage m -> m Text
fmtRichMessageDefault msg = fmtRichMessageCustom msg formatRichMessage
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
[03 05 2019 05:23:19.058] [Main.example#34] [ThreadId 11] app: First message...
[03 05 2019 05:23:19.059] [Main.example#35] [ThreadId 11] app: Second message...
@

Practically, it formats a message as 'fmtRichMessageDefault' without the severity information.
-}
fmtSimpleRichMessageDefault :: MonadIO m => RichMessage m -> m Text
fmtSimpleRichMessageDefault msg = fmtRichMessageCustom msg formatRichMessage
   where
    formatRichMessage :: Maybe ThreadId -> Maybe C.Time -> Message -> Text
    formatRichMessage (maybe "" showThreadId -> thread) (maybe "" showTime -> time) Msg{..} =
        time
     <> showSourceLoc msgStack
     <> thread
     <> msgText

fmtRichMessageCustom :: MonadIO m => RichMessage m -> (Maybe ThreadId -> Maybe C.Time -> Message -> Text) -> m Text
fmtRichMessageCustom RichMessage{..} formatter = do
    maybeThreadId  <- extractField $ TM.lookup @"threadId"  richMsgMap
    maybePosixTime <- extractField $ TM.lookup @"posixTime" richMsgMap
    pure $ formatter maybeThreadId maybePosixTime richMsgMsg

showTime :: C.Time -> Text
showTime t =
            square
          $ toStrict
          $ TB.toLazyText
          $ C.builder_DmyHMS timePrecision datetimeFormat (C.timeToDatetime t)
        where
            timePrecision = C.SubsecondPrecisionFixed 3
            datetimeFormat = C.DatetimeFormat (Just '-') (Just ' ') (Just ':')

showThreadId :: ThreadId -> Text
showThreadId = square . T.pack . show

{- | Allows to extend basic 'Message' type with given dependent map of fields.
-}
upgradeMessageAction
    :: forall m .
       FieldMap m
    -> LogAction m (RichMessage m)
    -> LogAction m Message
upgradeMessageAction fieldMap = cmap addMap
  where
    addMap :: Message -> RichMessage m
    addMap msg = RichMessage msg fieldMap
