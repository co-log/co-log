{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Provides type class for objects that has access to 'LogAction'.
-}

module Colog.Core.Class
       ( HasLog (..)
       , overLogAction
       ) where

import Colog.Core.Action (LogAction)

{- | This types class contains simple pair of getter-setter.

TODO: laws
-}
class HasLog env msg m where
    getLogAction :: env -> LogAction m msg
    setLogAction :: LogAction m msg -> env -> env

instance HasLog (LogAction m msg) msg m where
    getLogAction = id
    setLogAction = const

overLogAction :: HasLog env msg m => (LogAction m msg -> LogAction m msg) -> env -> env
overLogAction over env = setLogAction (over $ getLogAction env) env
{-# INLINE overLogAction #-}
