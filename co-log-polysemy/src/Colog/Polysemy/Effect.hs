{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright:  (c) 2019-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains logging effect which can be interpreted in terms of
'LogAction' from the @co-log-core@ package.
-}

module Colog.Polysemy.Effect
       ( -- * Effect
         Log (..)

         -- * Actions
       , log

         -- * Direct usages
       , LogActionSem
       , runLogActionSem

         -- * Interpretations
       , runLogAction
       , runLogAsTrace
       , runLogAsOutput

         -- * Interpretations for Other Effects
       , runTraceAsLog
       , runOutputAsLog

       ) where

import Prelude hiding (log)

import Data.Kind (Type)
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem_)
import Polysemy.Output (Output (..), output)
import Polysemy.Trace (Trace (..), trace)

import Colog.Core.Action (LogAction (..))


{- | Effect responsible for logging messages of type @msg@. Has similar
structure to 'LogAction'.

You can think of this effect in the following way in terms of the existing
effects in @polysemy@:

1. Like 'Output' but specialised for logging.
2. Like 'Trace' but polymorphic over logged message.
-}
data Log (msg :: Type) (m :: Type -> Type) (a :: Type) where
    Log :: msg -> Log msg m ()

makeSem_ ''Log

{- | Log a message. If you semantic monad 'Sem' has effect 'Log' then you can
send messages of type @msg@ to that effect. This function can be used like this:

@
application :: 'Member' ('Log' String) r => 'Sem' r ()
application = __do__
    'log' "Application started..."
    'log' "Application finished..."
@
-}
log :: forall msg r .
       Member (Log msg) r
    => msg       -- ^ Message to log
    -> Sem r ()  -- ^ Effectful computation with no result

{- | 'LogAction' that works directly with the 'Sem' monad.

@since 0.0.1.0
-}
type LogActionSem r msg = LogAction (Sem r) msg

{- | Run 'Sem' action with the corresponding 'LogActionSem'. If you
have plain 'LogAction' that works with some monad @m@, use
'runLogAction' instead.

@since 0.0.1.0
-}
runLogActionSem :: forall msg r a . LogActionSem r msg -> Sem (Log msg ': r) a -> Sem r a
runLogActionSem (LogAction action) = interpret $ \case
    Log msg -> action msg

{- | Run a 'Log' effect in terms of the given 'LogAction'. The idea behind this
function is the following: if you have @'LogAction' m msg@ then you can use this
action to tell how to io interpret effect 'Log'. However, this is only possible
if you also have @'Lift' m@ effect because running log action requires access to
monad @m@.

This function allows to use extensible effects provided by the @polysemy@
library with logging provided by @co-log@. You can construct 'LogAction'
independently and then just pass to this function to tell how to log messages.

Several examples:

1. @'runLogAction' mempty@: interprets the 'Log' effect by ignoring all messages.
2. @'runLogAction' 'Colog.Core.IO.logStringStdout'@: interprets 'Log' effect by
   allowing to log 'String' to @stdout@.
-}
runLogAction
    :: forall m msg r a .
       Member (Embed m) r
    => LogAction m msg
    -> Sem (Log msg ': r) a
    -> Sem r a
runLogAction (LogAction action) = interpret $ \case
    Log msg -> embed $ action msg
{-# INLINE runLogAction #-}

{- | Run 'Log' as the 'Trace' effect. This function can be useful if you have an
interpreter for the 'Trace' effect and you want to log strings using that
interpreter.
-}
runLogAsTrace
    :: forall r a .
       Member Trace r
    => Sem (Log String ': r) a
    -> Sem r a
runLogAsTrace = interpret $ \case
   Log msg -> trace msg
{-# INLINE runLogAsTrace #-}

{- | Run 'Log' as the 'Output' effect. This function can be useful if you have an
interpreter for the 'Output' effect and you want to log messages using that
interpreter.
-}
runLogAsOutput
    :: forall msg r a .
       Member (Output msg) r
    => Sem (Log msg ': r) a
    -> Sem r a
runLogAsOutput = interpret $ \case
   Log msg -> output msg
{-# INLINE runLogAsOutput #-}

{- | Run 'Trace' as the 'Log' effect. This function can be useful if you have an
interpreter for the 'Log' effect and you want to log strings using that
interpreter.
-}
runTraceAsLog
    :: forall r a .
       Member (Log String) r
    => Sem (Trace ': r) a
    -> Sem r a
runTraceAsLog = interpret $ \case
   Trace msg -> log msg
{-# INLINE runTraceAsLog #-}

{- | Run 'Output' as the 'Log' effect. This function can be useful if you have an
interpreter for the 'Log' effect and you want to log messages using that
interpreter.
-}
runOutputAsLog
    :: forall msg r a .
       Member (Log msg) r
    => Sem (Output msg ': r) a
    -> Sem r a
runOutputAsLog = interpret $ \case
   Output msg -> log msg
{-# INLINE runOutputAsLog #-}
