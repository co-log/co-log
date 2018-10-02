{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | This module contains 'LogAction' instances of @contravariant@ classes.
-}

module Colog.Contra
       (
       ) where

#if !MIN_VERSION_base_noprelude(4,12,0)
import Data.Functor.Contravariant (Contravariant (..))
#endif
import Data.Functor.Contravariant.Divisible (Decidable (..), Divisible (..))

import Colog.Core.Action (LogAction)
import qualified Colog.Core.Action as LA

#if !MIN_VERSION_base_noprelude(4,12,0)
instance Contravariant (LogAction m) where
    contramap = LA.cmap
    (>$)      = (LA.>$)
#endif

instance (Applicative m) => Divisible (LogAction m) where
    divide  = LA.divide
    conquer = LA.conquer

instance (Applicative m) => Decidable (LogAction m) where
    lose   = LA.lose
    choose = LA.choose
