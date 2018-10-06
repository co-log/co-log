module Colog.Core.IO 
    ( logStringStdout
    , logStringStderr
    , logStringHandle
    , withLogStringFile
    , liftLogIO
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (Handle, IOMode( AppendMode ), hPutStrLn, stderr, withFile)
import Colog.Core.Action (LogAction (..))

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

{- | Action that prints 'String' to 'Handle'. -}
logStringHandle :: MonadIO m => Handle -> LogAction m String
logStringHandle handle = LogAction $ liftIO . hPutStrLn handle

{- | Action that prints 'String' to file. Instead of returning 'LogAction' it's
implemented in continuation-passing style because it's more efficient to open
file only once at the start of the application and write to 'Handle' instead of
opening file each time we need to write to it.

Opens file in 'AppendMode'.
-}
withLogStringFile :: MonadIO m => FilePath -> (LogAction m String -> IO r) -> IO r
withLogStringFile path action = withFile path AppendMode $ action . logStringHandle

{- | Lifts a LogAction over IO into a more general Monad. -}
liftLogIO :: MonadIO m => LogAction IO msg -> LogAction m msg
liftLogIO (LogAction action) = LogAction (liftIO . action)