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

----------------------------------------------------------------------------
-- Logger rotation
----------------------------------------------------------------------------

{-
data Limit = LimitTo Natural | Unlimited

{- | Logger rotation action. Takes name of the logging file @file.foo@. Always
writes new logs to file named @file.foo@ (given file name, also called as /hot log/).

* If the size of the file exceeds given limit for file sizes then this action
  renames @file.foo@ to @file.foo.(n + 1)@ (where @n@ is the number of latest
  renamed file).
* If the number of files on the filesystem is bigger than the files number limit
  then the given @FilePath -> IO ()@ action is called on the oldest file. As
  simple solution, you can pass @removeFile@ function to delete old files but
  you can also pass some archiving function if you don't want to loose old logs.
-}
withLogRotation
    :: forall msg m .
       MonadIO m
    => Limit  -- TODO: use 'named' library here to distinguish limits?
    -- ^ Max allowed file size in bytes
    -> Limit
    -- ^ Max allowed number of files to have
    -> FilePath
    -- ^ File path to log
    -> (FilePath -> IO ())
    -- ^ What to do with old files; pass @removeFile@ here for deletion
    -> (Handle -> LogAction m msg)
    -- ^ Action that writes to file handle
    -> (LogAction m msg -> IO r)
    -- ^ Continuation action
    -> IO r
withLogRotation sizeLimit filesLimit path cleanup mkAction cont = cont rotationAction
  where
    rotationAction :: LogAction m msg
    rotationAction = LogAction $ \msg -> do
        withFile path AppendMode writeFileLoop
-}
