{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- | Provides type class for objects that has access to 'LogAction'.
-}

module Colog.Core.Class
       ( HasLog (..)
       ) where

import Colog.Core.Action (LogAction)

{- | This types class contains simple pair of getter-setter.

TODO: laws
-}
class HasLog env msg m where
    {-# MINIMAL getLogAction, (setLogAction | overLogAction) #-}
    getLogAction :: env -> LogAction m msg
    setLogAction :: LogAction m msg -> env -> env
    setLogAction = overLogAction . const
    {-# INLINE setLogAction #-}
    overLogAction :: (LogAction m msg -> LogAction m msg) -> env -> env
    overLogAction f env = setLogAction (f $ getLogAction env) env
    {-# INLINE overLogAction #-}

instance HasLog (LogAction m msg) msg m where
    {-# INLINE getLogAction #-}
    getLogAction = id
    {-# INLINE setLogAction #-}
    setLogAction = const
    {-# INLINE overLogAction #-}
    overLogAction = id
