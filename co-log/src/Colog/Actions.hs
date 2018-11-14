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

import Control.Monad.IO.Class (MonadIO (..))
import System.IO (Handle, IOMode (AppendMode), stderr, withFile)

import Colog.Core.Action (LogAction (..))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

----------------------------------------------------------------------------
-- ByteString
----------------------------------------------------------------------------

{- | Action that prints 'ByteString' to stdout. -}
logByteStringStdout :: MonadIO m => LogAction m BS.ByteString
logByteStringStdout = LogAction $ liftIO . BS8.putStrLn

{- | Action that prints 'ByteString' to stderr. -}
logByteStringStderr :: MonadIO m => LogAction m BS.ByteString
logByteStringStderr = logByteStringHandle stderr

{- | Action that prints 'ByteString' to 'Handle'. -}
logByteStringHandle :: MonadIO m => Handle -> LogAction m BS.ByteString
logByteStringHandle handle = LogAction $ liftIO . BS8.hPutStrLn handle

{- | Action that prints 'ByteString' to file. See 'withLogStringFile' for details. -}
withLogByteStringFile :: MonadIO m => FilePath -> (LogAction m BS.ByteString -> IO r) -> IO r
withLogByteStringFile path action = withFile path AppendMode $ action . logByteStringHandle

----------------------------------------------------------------------------
-- Text
----------------------------------------------------------------------------

{- | Action that prints 'Text' to stdout. -}
logTextStdout :: MonadIO m => LogAction m T.Text
logTextStdout = LogAction $ liftIO . TIO.putStrLn

{- | Action that prints 'Text' to stderr. -}
logTextStderr :: MonadIO m => LogAction m T.Text
logTextStderr = logTextHandle stderr

{- | Action that prints 'Text' to 'Handle'. -}
logTextHandle :: MonadIO m => Handle -> LogAction m T.Text
logTextHandle handle = LogAction $ liftIO . TIO.hPutStrLn handle

{- | Action that prints 'Text' to file. See 'withLogStringFile' for details. -}
withLogTextFile :: MonadIO m => FilePath -> (LogAction m T.Text -> IO r) -> IO r
withLogTextFile path action = withFile path AppendMode $ action . logTextHandle

