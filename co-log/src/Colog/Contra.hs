{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | This module contains 'LogAction' instances of @contravariant@ classes.
-}

module Colog.Contra
       (
       ) where

import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Decidable (..), Divisible (..))

import Colog.Core.Action (LogAction)

import qualified Colog.Core.Action as LA

instance Contravariant (LogAction m) where
    contramap = LA.cmap
    (>$)      = (LA.>$)

instance (Applicative m) => Divisible (LogAction m) where
    divide  = LA.divide
    conquer = LA.conquer

instance (Applicative m) => Decidable (LogAction m) where
    lose   = LA.lose
    choose = LA.choose
