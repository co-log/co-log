module Colog.Actions
       ( -- * 'String' actions
         logStringStdout
       , logStringStderr
       , logStringHandle
       , withLogStringFile

         -- * 'ByteString' actions
       , logByteStringStdout
       , logByteStringStderr
       , logByteStringHandle
       , withLogByteStringFile

         -- * 'Text' actions
       , logTextStdout
       , logTextStderr
       , logTextHandle
       , withLogTextFile
       ) where

import System.IO (hPutStrLn)

import Colog.Core.Action (LogAction (..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as TIO

----------------------------------------------------------------------------
-- String
----------------------------------------------------------------------------

{- | Action that prints 'String' to stdout. -}
logStringStdout :: MonadIO m => LogAction m String
logStringStdout = LogAction putStrLn

{- | Action that prints 'String' to stderr. -}
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

----------------------------------------------------------------------------
-- ByteString
----------------------------------------------------------------------------

{- | Action that prints 'ByteString' to stdout. -}
logByteStringStdout :: MonadIO m => LogAction m ByteString
logByteStringStdout = LogAction putStrLn

{- | Action that prints 'ByteString' to stderr. -}
logByteStringStderr :: MonadIO m => LogAction m ByteString
logByteStringStderr = logByteStringHandle stderr

{- | Action that prints 'ByteString' to 'Handle'. -}
logByteStringHandle :: MonadIO m => Handle -> LogAction m ByteString
logByteStringHandle handle = LogAction $ liftIO . BS.hPutStrLn handle

{- | Action that prints 'ByteString' to file. See 'withLogStringFile' for details. -}
withLogByteStringFile :: MonadIO m => FilePath -> (LogAction m ByteString -> IO r) -> IO r
withLogByteStringFile path action = withFile path AppendMode $ action . logByteStringHandle

----------------------------------------------------------------------------
-- Text
----------------------------------------------------------------------------

{- | Action that prints 'Text' to stdout. -}
logTextStdout :: MonadIO m => LogAction m Text
logTextStdout = LogAction putTextLn

{- | Action that prints 'Text' to stderr. -}
logTextStderr :: MonadIO m => LogAction m Text
logTextStderr = logTextHandle stderr

{- | Action that prints 'Text' to 'Handle'. -}
logTextHandle :: MonadIO m => Handle -> LogAction m Text
logTextHandle handle = LogAction $ liftIO . TIO.hPutStrLn handle

{- | Action that prints 'Text' to file. See 'withLogStringFile' for details. -}
withLogTextFile :: MonadIO m => FilePath -> (LogAction m Text -> IO r) -> IO r
withLogTextFile path action = withFile path AppendMode $ action . logTextHandle
