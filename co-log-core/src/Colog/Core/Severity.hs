{-# LANGUAGE PatternSynonyms #-}

{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module introduces 'Severity' data type for expressing how severe the
message is. Also, it contains useful functions and patterns for work with 'Severity'.


 +-----------+---------+-----------------------------------------+-----------------------------+
 | Severity  | Pattern | Meaning                                 | Example                     |
 +===========+=========+=========================================+=============================+
 | 'Debug'   | 'D'     | Information useful for debug purposes   | Internal function call logs |
 +-----------+---------+-----------------------------------------+-----------------------------+
 | 'Info'    | 'I'     | Normal operational information          | Finish file uploading       |
 +-----------+---------+-----------------------------------------+-----------------------------+
 | 'Warning' | 'W'     | General warnings, non-critical failures | Image load error            |
 +-----------+---------+-----------------------------------------+-----------------------------+
 | 'Error'   | 'E'     | General errors/severe errors            | Could not connect to the DB |
 +-----------+---------+-----------------------------------------+-----------------------------+
-}

module Colog.Core.Severity
       ( Severity (..)
         -- ** Patterns
         -- $pattern
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

{- $pattern
Instead of using full names of the constructors you can instead use one-letter
patterns. To do so you can import and use the pattern:

@
__import__ Colog (__pattern__ D)

example :: WithLog env Message m => m ()
example = log D "I'm using severity pattern"
@

Moreover, you could use patterns when pattern-matching on severity

@
errorToStderr :: 'Severity' -> IO ()
errorToStderr E = hputStrLn stderr "Error severity"
errorToStderr _ = putStrLn "Something else"
@
-}

pattern D, I, W, E :: Severity
pattern D <- Debug   where D = Debug
pattern I <- Info    where I = Info
pattern W <- Warning where W = Warning
pattern E <- Error   where E = Error
{-# COMPLETE D, I, W, E #-}

-- | Filters messages by the given 'Severity'.
filterBySeverity
    :: Applicative m
    => Severity
    -> (a -> Severity)
    -> LogAction m a
    -> LogAction m a
filterBySeverity s fs = cfilter (\a -> fs a >= s)
{-# INLINE filterBySeverity #-}
