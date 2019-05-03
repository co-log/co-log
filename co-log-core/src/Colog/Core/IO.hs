{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Introduces logging actions working in 'MonadIO'.
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
       ) where

import Colog.Core.Action (LogAction (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, IOMode (AppendMode), hPrint, hPutStrLn, stderr, withFile)

----------------------------------------------------------------------------
-- String
----------------------------------------------------------------------------

{- | Action that prints 'String' to stdout.

>>> unLogAction logStringStdout "foo"
foo
-}
logStringStdout :: MonadIO m => LogAction m String
logStringStdout = LogAction (liftIO . putStrLn)

{- | Action that prints 'String' to stderr.

>>> unLogAction logStringStderr "foo"
foo
-}
logStringStderr :: MonadIO m => LogAction m String
logStringStderr = logStringHandle stderr

{- | Action that prints 'String' to 'Handle'.

>>> unLogAction (logStringHandle stderr) "foo"
foo
-}
logStringHandle :: MonadIO m => Handle -> LogAction m String
logStringHandle handle = LogAction $ liftIO . hPutStrLn handle

{- | Action that prints 'String' to file. Instead of returning 'LogAction' it's
implemented in continuation-passing style because it's more efficient to open
file only once at the start of the application and write to 'Handle' instead of
opening file each time we need to write to it.

Opens file in 'AppendMode'.

>>> logger action = unLogAction action "foo"
>>> withLogStringFile "/dev/stdout" logger
foo
-}
withLogStringFile :: MonadIO m => FilePath -> (LogAction m String -> IO r) -> IO r
withLogStringFile path action = withFile path AppendMode $ action . logStringHandle

----------------------------------------------------------------------------
-- Show
----------------------------------------------------------------------------

{- | Action that prints to stdout using 'Show'.

>>> unLogAction logPrint 5
5
-}
logPrint :: forall a m . (Show a, MonadIO m) => LogAction m a
logPrint = LogAction $ liftIO . print

{- | Action that prints to stderr using 'Show'.

>>> unLogAction logPrintStderr 5
5
-}
logPrintStderr :: forall a m . (Show a, MonadIO m) => LogAction m a
logPrintStderr = logPrintHandle stderr

{- | Action that prints to a 'Handle' using 'Show'.

>>> unLogAction (logPrintHandle stderr) 5
5
-}
logPrintHandle :: forall a m . (Show a, MonadIO m) => Handle -> LogAction m a
logPrintHandle handle = LogAction $ liftIO . hPrint handle

{- | Action that prints to a file using 'Show'. See 'withLogStringFile' for details.
-}
withLogPrintFile :: forall a m r . (Show a, MonadIO m) => FilePath -> (LogAction m a -> IO r) -> IO r
withLogPrintFile path action = withFile path AppendMode $ action . logPrintHandle

----------------------------------------------------------------------------
-- Misc
----------------------------------------------------------------------------

{- | Lifts a LogAction over IO into a more general Monad.

>>> logToStdout = LogAction putStrLn
>>> unLogAction (liftLogIO logToStdout) "foo"
foo
-}
liftLogIO :: MonadIO m => LogAction IO msg -> LogAction m msg
liftLogIO (LogAction action) = LogAction (liftIO . action)
