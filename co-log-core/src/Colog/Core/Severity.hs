module Colog.Core.Severity
       ( Severity (..)
       , filterSeverity
       ) where

import Colog.Core.Action (LogAction (..), cfilter)

-- | Severity for the log messages.
data Severity
    -- | Information useful for debug purposes
    = Debug
    -- | Normal operational information
    | Info
    -- | Important (more than average) but not critical information
    | Notice
    -- | General warnings
    | Warning
    -- | General errors/severe errors
    | Error
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- Filters by given 'Severity'.
filterSeverity :: Applicative m => Severity -> (a -> Severity) -> LogAction m a -> LogAction m a
filterSeverity s fs = cfilter (\a -> fs a >= s)
