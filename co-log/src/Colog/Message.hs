{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

{- | 'Message' with 'Severity', and logging functions for them.
-}

module Colog.Message
       ( -- * Basic message type
         Message (..)
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
       , defaultMessageMap

       , RichMessage
       , fmtRichMessageDefault
       , upgradeMessageAction
       ) where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (displayException)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.TypeRepMap (TypeRepMap)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Stack (SrcLoc (..))
import GHC.TypeLits (KnownSymbol, Symbol)
import System.Console.ANSI (Color (..), ColorIntensity (Vivid), ConsoleLayer (Foreground), SGR (..),
                            setSGRCode)

import Colog.Core (LogAction, Severity (..), cmap)
import Colog.Monad (WithLog, logMsg)

import qualified Data.TypeRepMap as TM

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
logException = withFrozenCallStack (logError . toText . displayException)

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
        toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine

----------------------------------------------------------------------------
-- Externally extensible message
----------------------------------------------------------------------------

{- | Open type family that maps some user defined tags (type names) to actual
types. The type family is open so you can add new instances.
-}
type family FieldType (fieldName :: Symbol) :: Type
type instance FieldType "threadId" = ThreadId
type instance FieldType "utcTime"  = UTCTime

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
    MessageField
        :: forall fieldName m .
           { unMesssageField :: m (FieldType fieldName) }
        -> MessageField m fieldName

instance (KnownSymbol fieldName, a ~ m (FieldType fieldName))
      => IsLabel fieldName (a -> TM.WrapTypeable (MessageField m)) where
    fromLabel field = TM.WrapTypeable $ MessageField @fieldName field

extractField
    :: Applicative m
    => Maybe (MessageField m fieldName)
    -> m (Maybe (FieldType fieldName))
extractField = traverse unMesssageField

-- same as:
-- extractField = \case
--    Nothing -> pure Nothing
--    Just (MessageField field) -> Just <$> field

{- | Depedent map from type level strings to the corresponding types. See
'FieldType' for mapping between names and types.
-}
type FieldMap (m :: Type -> Type) = TypeRepMap (MessageField m)

{- | Default message map that contains actions to extract 'ThreadId' and
'UTCTime'. Basically, the following mapping:

@
"threadId" -> myThreadId
"utcTime"  -> getCurrentTime
@
-}
defaultMessageMap :: MonadIO m => FieldMap m
defaultMessageMap = fromList
    [ #threadId (liftIO myThreadId)
    , #utcTime  (liftIO getCurrentTime)
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
    maybeUtcTime  <- extractField $ TM.lookup @"utcTime"  richMessageMap
    pure $ formatRichMessage maybeThreadId maybeUtcTime richMessageMsg
  where
    formatRichMessage :: Maybe ThreadId -> Maybe UTCTime -> Message -> Text
    formatRichMessage (maybe "" showThreadId -> thread) (maybe "" showTime -> time) Message{..} =
        showSeverity messageSeverity
     <> time
     <> showSourceLoc messageStack
     <> thread
     <> messageText

    showTime :: UTCTime -> Text
    showTime t = square $ toText $
          formatTime defaultTimeLocale "%H:%M:%S." t
       ++ take 3 (formatTime defaultTimeLocale "%q" t)
       ++ formatTime defaultTimeLocale " %e %b %Y %Z" t

    showThreadId :: ThreadId -> Text
    showThreadId = square . show

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
