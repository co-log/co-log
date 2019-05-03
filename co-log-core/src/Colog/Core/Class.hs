{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Provides type class for objects that has access to 'LogAction'.
-}

module Colog.Core.Class
       ( HasLog (..)

         -- * Lens
         -- $lens
       , Lens'
       ) where

import Colog.Core.Action (LogAction)


{- | This types class contains simple pair of getter-setter and related
functions.
It also provides the useful lens 'logActionL' with the default implementation using type
class methods. The default one could be easily overritten under your instances.

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

    logActionL :: Lens' env (LogAction m msg)
    logActionL = lens getLogAction (flip setLogAction)
    {-# INLINE logActionL #-}

instance HasLog (LogAction m msg) msg m where
    {-# INLINE getLogAction #-}
    getLogAction = id
    {-# INLINE setLogAction #-}
    setLogAction = const
    {-# INLINE overLogAction #-}
    overLogAction = id

----------------------------------------------------------------------------
-- Lens
----------------------------------------------------------------------------

{- $lens
To keep @co-log-core@ a lightweight library it was decided to introduce local
'Lens'' type alias as it doesn't harm.
-}

{- | The monomorphic lenses which don't change the type of the container (or of
the value inside).
-}
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | Creates 'Lens'' from the getter and setter.
lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens getter setter = \f s -> setter s <$> f (getter s)
{-# INLINE lens #-}
