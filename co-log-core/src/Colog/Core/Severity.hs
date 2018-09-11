{-# LANGUAGE PatternSynonyms #-}

{- | This module introduces 'Severity' data type for expressing how severe the
message is. Also, it contains useful functions for work with 'Severity'.

-}

module Colog.Core.Severity
       ( Severity (..)
         -- Patterns
       , pattern D
       , pattern I
       , pattern W
       , pattern E
       , filterBySeverity
       ) where

import Data.Ix (Ix)

import Colog.Core.Action (LogAction (..), cfilter)

-- | Severity for the log messages.
data Severity
    {- | Information useful for debug purposes.

    E.g. output of the function that is important for the internal development,
    not for users. Like, the result of SQL query.
    -}
    = Debug
    {- | Normal operational information.

    E.g. describing general steps: starting application, finished downloading.
    -}
    | Info
    {- | General warnings, non-critical failures.

    E.g. couldn't download icon from some service to display.
    -}
    | Warning
    {- | General errors/severe errors.

    E.g. exceptional situations: couldn't syncronize accounts.
    -}
    | Error
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Ix)

pattern D, I, W, E :: Severity
pattern D <- Debug   where D = Debug
pattern I <- Info    where I = Info
pattern W <- Warning where W = Warning
pattern E <- Error   where E = Error
{-# COMPLETE D, I, W, E #-}


-- | Filters messages by given 'Severity'.
filterBySeverity :: Applicative m => Severity -> (a -> Severity) -> LogAction m a -> LogAction m a
filterBySeverity s fs = cfilter (\a -> fs a >= s)
