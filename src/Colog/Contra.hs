{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP #-}

{- |
Copyright:  (c) 2018-2022 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains 'LogAction' orphan instances of @contravariant@ classes.
-}

module Colog.Contra
       (
       ) where

#if !MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant (Contravariant (..))
#endif
import Data.Functor.Contravariant.Divisible (Decidable (..), Divisible (..))

import Colog.Core.Action (LogAction)

import qualified Colog.Core.Action as LA


#if !MIN_VERSION_base(4,12,0)
instance Contravariant (LogAction m) where
    contramap = LA.cmap
    {-# INLINE contramap #-}
    (>$) = (LA.>$)
    {-# INLINE (>$) #-}
#endif

instance (Applicative m) => Divisible (LogAction m) where
    divide  = LA.divide
    {-# INLINE divide #-}
    conquer = LA.conquer
    {-# INLINE conquer #-}

instance (Applicative m) => Decidable (LogAction m) where
    lose   = LA.lose
    {-# INLINE lose #-}
    choose = LA.choose
    {-# INLINE choose #-}
