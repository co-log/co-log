{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeFamilies          #-}

{- | 'Message' with 'Severity', and logging functions for them.
-}

module Colog.Message
       ( -- * Basic message type
         Message (..)
       , unMessageField
       , log
       , logDebug
       , logInfo
       , logWarning
       , logError
       , logException
       , fmtMessage

         -- * Externally extensible message type
       , FieldType
       , MessageField (..)
       , FieldMap
       , defaultFieldMap

       , RichMessage
       , fmtRichMessageDefault
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

import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.TypeRepMap as TM
import qualified Chronos as C

----------------------------------------------------------------------------
-- Plain message
----------------------------------------------------------------------------

-- | Consist of the message 'Severity' level and the message itself.
data Message = Message
    { messageSeverity :: !Severity
    , messageStack    :: !CallStack
    , messageText     :: !Text
    }

-- | Logs the message with given 'Severity'.
log :: WithLog env Message m => Severity -> Text -> m ()
log messageSeverity messageText =
    withFrozenCallStack (logMsg Message{ messageStack = callStack, .. })

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

-- | Logs 'Exception' message.
logException :: forall e m env . (WithLog env Message m, Exception e) => e -> m ()
logException = withFrozenCallStack (logError . T.pack . displayException)

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
    color c txt = T.pack (setSGRCode [SetColor Foreground Vivid c])
        <> txt
        <> T.pack (setSGRCode [Reset])

square :: Text -> Text
square t = "[" <> t <> "] "

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

unMessageField :: forall fieldName m . MessageField m fieldName -> m (FieldType fieldName)
unMessageField (MessageField f) = f

instance (KnownSymbol fieldName, a ~ m (FieldType fieldName))
      => IsLabel fieldName (a -> TM.WrapTypeable (MessageField m)) where
#if MIN_VERSION_base(4,11,0)
    fromLabel field = TM.WrapTypeable $ MessageField @fieldName field
#else
    fromLabel field = TM.WrapTypeable $ MessageField  @_ @fieldName field
#endif
    {-# INLINE fromLabel #-}

extractField
    :: Applicative m
    => Maybe (MessageField m fieldName)
    -> m (Maybe (FieldType fieldName))
extractField = traverse unMessageField

-- same as:
-- extractField = \case
--    Nothing -> pure Nothing
--    Just (MessageField field) -> Just <$> field

{- | Depedent map from type level strings to the corresponding types. See
'FieldType' for mapping between names and types.
-}
type FieldMap (m :: Type -> Type) = TypeRepMap (MessageField m)

{- | Default message map that contains actions to extract 'ThreadId' and
'posixTime'. Basically, the following mapping:

@
"threadId" -> myThreadId
"posixTime"  -> getCurrentTime
@
-}
defaultFieldMap :: MonadIO m => FieldMap m
defaultFieldMap = fromList
    [ #threadId (liftIO myThreadId)
    , #posixTime  (liftIO C.now)
    ]

-- | Contains additional data to 'Message' to display more verbose information.
data RichMessage (m :: Type -> Type) = RichMessage
    { richMessageMsg :: {-# UNPACK #-} !Message
    , richMessageMap :: {-# UNPACK #-} !(FieldMap m)
    }

{- | Formats 'RichMessage' in the following way:

@
[Severity] [Time] [SourceLocation] [ThreadId] <Text message>
@
-}
fmtRichMessageDefault :: MonadIO m => RichMessage m -> m Text
fmtRichMessageDefault RichMessage{..} = do
    maybeThreadId <- extractField $ TM.lookup @"threadId" richMessageMap
    maybePosixTime  <- extractField $ TM.lookup @"posixTime"  richMessageMap
    pure $ formatRichMessage maybeThreadId maybePosixTime richMessageMsg
  where
    formatRichMessage :: Maybe ThreadId -> Maybe C.Time -> Message -> Text
    formatRichMessage (maybe "" showThreadId -> thread) (maybe "" showTime -> time) Message{..} =
        showSeverity messageSeverity
     <> time
     <> showSourceLoc messageStack
     <> thread
     <> messageText

    showTime :: C.Time -> Text
    showTime t = square $ toStrict $ TB.toLazyText $ C.builder_DmyHMS timePrecision datetimeFormat (C.timeToDatetime t)
      where
        timePrecision = C.SubsecondPrecisionFixed 3
        datetimeFormat = C.DatetimeFormat (Just ' ') (Just ' ') (Just ':')

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
