module Colog.Actions
       (
         -- * 'ByteString' actions
         logByteStringStdout
       , logByteStringStderr
       , logByteStringHandle
       , withLogByteStringFile

         -- * 'Text' actions
       , logTextStdout
       , logTextStderr
       , logTextHandle
       , withLogTextFile
       ) where

import Colog.Core.Action (LogAction (..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as TIO

----------------------------------------------------------------------------
-- ByteString
----------------------------------------------------------------------------

{- | Action that prints 'ByteString' to stdout. -}
logByteStringStdout :: MonadIO m => LogAction m ByteString
logByteStringStdout = LogAction putBSLn

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

