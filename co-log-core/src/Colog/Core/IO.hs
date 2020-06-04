{-# LANGUAGE CPP #-}

{- |
Copyright:  (c) 2018-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Introduces logging actions working in 'MonadIO'. These actions are very basic
and inefficient because they use the 'String' data type. If you don't want to
have extra dependencies and performance of logging is not the bottleneck of your
application, then these functions should be enough. Otherwise use functions from
the "Colog.Actions" module from the @co-log@ package.
-}

module Colog.Core.IO
       ( -- * 'String' actions
         logStringStdout
       , logStringStderr
       , logStringHandle
       , withLogStringFile

         -- * 'Show' actions
       , logPrint
       , logPrintStderr
       , logPrintHandle
       , withLogPrintFile

         -- * Various combinators
       , liftLogIO
       , logFlush
       ) where

import Colog.Core.Action (LogAction (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, IOMode (AppendMode), hFlush, hPrint, hPutStrLn, stderr, withFile)


{- $setup
>>> import Colog.Core.Action
-}

----------------------------------------------------------------------------
-- String
----------------------------------------------------------------------------

{- | Action that prints 'String' to stdout.

>>> logStringStdout <& "foo"
foo
-}
logStringStdout :: MonadIO m => LogAction m String
logStringStdout = LogAction (liftIO . putStrLn)
{-# INLINE logStringStdout #-}
{-# SPECIALIZE logStringStdout :: LogAction IO String #-}

{- | Action that prints 'String' to stderr.

>>> logStringStderr <& "foo"
foo
-}
logStringStderr :: MonadIO m => LogAction m String
logStringStderr = logStringHandle stderr
{-# INLINE logStringStderr #-}
{-# SPECIALIZE logStringStderr :: LogAction IO String #-}

{- | Action that prints 'String' to 'Handle'.

>>> logStringHandle stderr <& "foo"
foo
-}
logStringHandle :: MonadIO m => Handle -> LogAction m String
logStringHandle handle = LogAction $ liftIO . hPutStrLn handle
{-# INLINE logStringHandle #-}
{-# SPECIALIZE logStringHandle :: Handle -> LogAction IO String #-}

{- | Action that prints 'String' to file. Instead of returning 'LogAction' it's
implemented in continuation-passing style because it's more efficient to open
file only once at the start of the application and write to 'Handle' instead of
opening file each time we need to write to it.

Opens file in 'AppendMode'.

#ifndef mingw32_HOST_OS

>>> logger action = action <& "foo"
>>> withLogStringFile "/dev/stdout" logger
foo

#endif
-}
withLogStringFile :: MonadIO m => FilePath -> (LogAction m String -> IO r) -> IO r
withLogStringFile path action = withFile path AppendMode $ action . logStringHandle
{-# INLINE withLogStringFile #-}
{-# SPECIALIZE withLogStringFile :: FilePath -> (LogAction IO String -> IO r) -> IO r #-}

----------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------

{- | Action that prints to stdout using 'Show'.

>>> logPrint <& 5
5
-}
logPrint :: forall a m . (Show a, MonadIO m) => LogAction m a
logPrint = LogAction $ liftIO . print
{-# INLINE logPrint #-}
{-# SPECIALIZE logPrint :: Show a => LogAction IO a #-}

{- | Action that prints to stderr using 'Show'.

>>> logPrintStderr <& 5
5
-}
logPrintStderr :: forall a m . (Show a, MonadIO m) => LogAction m a
logPrintStderr = logPrintHandle stderr
{-# INLINE logPrintStderr #-}
{-# SPECIALIZE logPrintStderr :: Show a => LogAction IO a #-}

{- | Action that prints to a 'Handle' using 'Show'.

>>> logPrintHandle stderr <& 5
5
-}
logPrintHandle :: forall a m . (Show a, MonadIO m) => Handle -> LogAction m a
logPrintHandle handle = LogAction $ liftIO . hPrint handle
{-# INLINE logPrintHandle #-}
{-# SPECIALIZE logPrintHandle :: Show a => Handle -> LogAction IO a #-}

{- | Action that prints to a file using 'Show'. See 'withLogStringFile' for details.
-}
withLogPrintFile
    :: forall a m r . (Show a, MonadIO m)
    => FilePath
    -> (LogAction m a -> IO r)
    -> IO r
withLogPrintFile path action = withFile path AppendMode $ action . logPrintHandle
{-# INLINE withLogPrintFile #-}
{-# SPECIALIZE withLogPrintFile :: Show a => FilePath -> (LogAction IO a -> IO r) -> IO r #-}

----------------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------------

{- | Lifts a LogAction over IO into a more general Monad.

>>> logToStdout = LogAction putStrLn
>>> liftLogIO logToStdout <& "foo"
foo
-}
liftLogIO :: MonadIO m => LogAction IO msg -> LogAction m msg
liftLogIO (LogAction action) = LogAction (liftIO . action)
{-# INLINE liftLogIO #-}

{- | This action can be used in combination with other actions to flush
a handle every time you log anything.

@since x.x.x.x
-}
logFlush :: MonadIO m => Handle -> LogAction m a
logFlush handle = LogAction $ const $ liftIO $ hFlush handle
{-# INLINE logFlush #-}
{-# SPECIALIZE logFlush :: Handle -> LogAction IO a #-}
