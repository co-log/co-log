{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Provides type class for values that has access to 'LogAction'.
-}

module Colog.Core.Class
       ( HasLog (..)

         -- * Lens
         -- $lens
       , Lens'
       ) where

import Colog.Core.Action (LogAction)


-- to inline lens better
{- HLINT ignore "Redundant lambda" -}

{- | This types class contains simple pair of getter-setter and related
functions.
It also provides the useful lens 'logActionL' with the default implementation using type
class methods. The default one could be easily overritten under your instances.

Every instance of the this typeclass should satisfy the following laws:

1. __Set-Get:__ @'getLogAction' ('setLogAction' l env) ≡ l@
2. __Get-Set:__ @'setLogAction' ('getLogAction' env) env ≡ env@
3. __Set-Set:__ @'setLogAction' l2 ('setLogAction' l1 env) ≡ 'setLogAction' l2 env@
4. __Set-Over:__ @'overLogAction' f env ≡ 'setLogAction' (f $ 'getLogAction' env) env@
-}
class HasLog env msg m where
    {-# MINIMAL getLogAction, (setLogAction | overLogAction) #-}

    -- | Extracts 'LogAction' from the environment.
    getLogAction :: env -> LogAction m msg

    -- | Sets 'LogAction' to the given one inside the environment.
    setLogAction :: LogAction m msg -> env -> env
    setLogAction = overLogAction . const
    {-# INLINE setLogAction #-}

    -- | Applies function to the 'LogAction' inside the environment.
    overLogAction :: (LogAction m msg -> LogAction m msg) -> env -> env
    overLogAction f env = setLogAction (f $ getLogAction env) env
    {-# INLINE overLogAction #-}

    -- | Lens for 'LogAction' inside the environment.
    logActionL :: Lens' env (LogAction m msg)
    logActionL = lens getLogAction (flip setLogAction)
    {-# INLINE logActionL #-}

instance HasLog (LogAction m msg) msg m where
    getLogAction :: LogAction m msg -> LogAction m msg
    getLogAction = id
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m msg -> LogAction m msg -> LogAction m msg
    setLogAction = const
    {-# INLINE setLogAction #-}

    overLogAction
        :: (LogAction m msg -> LogAction m msg)
        -> LogAction m msg
        -> LogAction m msg
    overLogAction = id
    {-# INLINE overLogAction #-}

    logActionL :: Lens' (LogAction m msg) (LogAction m msg)
    logActionL = \f s -> s <$ f s
    {-# INLINE logActionL #-}

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
