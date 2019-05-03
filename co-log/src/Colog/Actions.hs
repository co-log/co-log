{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Logging actions for various text types.
-}

module Colog.Actions
       ( -- * 'ByteString' actions
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
{-# INLINE logByteStringStdout #-}
{-# SPECIALIZE logByteStringStdout :: LogAction IO BS.ByteString #-}

{- | Action that prints 'ByteString' to stderr. -}
logByteStringStderr :: MonadIO m => LogAction m BS.ByteString
logByteStringStderr = logByteStringHandle stderr
{-# INLINE logByteStringStderr #-}
{-# SPECIALIZE logByteStringStderr :: LogAction IO BS.ByteString #-}

{- | Action that prints 'ByteString' to 'Handle'. -}
logByteStringHandle :: MonadIO m => Handle -> LogAction m BS.ByteString
logByteStringHandle handle = LogAction $ liftIO . BS8.hPutStrLn handle
{-# INLINE logByteStringHandle #-}
{-# SPECIALIZE logByteStringHandle :: Handle -> LogAction IO BS.ByteString #-}

{- | Action that prints 'ByteString' to file. See 'withLogStringFile' for details. -}
withLogByteStringFile :: MonadIO m => FilePath -> (LogAction m BS.ByteString -> IO r) -> IO r
withLogByteStringFile path action = withFile path AppendMode $ action . logByteStringHandle
{-# INLINE withLogByteStringFile #-}
{-# SPECIALIZE withLogByteStringFile :: FilePath -> (LogAction IO BS.ByteString -> IO r) -> IO r #-}

----------------------------------------------------------------------------
-- Text
----------------------------------------------------------------------------

{- | Action that prints 'Text' to stdout. -}
logTextStdout :: MonadIO m => LogAction m T.Text
logTextStdout = LogAction $ liftIO . TIO.putStrLn
{-# INLINE logTextStdout #-}
{-# SPECIALIZE logTextStdout :: LogAction IO T.Text #-}

{- | Action that prints 'Text' to stderr. -}
logTextStderr :: MonadIO m => LogAction m T.Text
logTextStderr = logTextHandle stderr
{-# INLINE logTextStderr #-}
{-# SPECIALIZE logTextStderr :: LogAction IO T.Text #-}

{- | Action that prints 'Text' to 'Handle'. -}
logTextHandle :: MonadIO m => Handle -> LogAction m T.Text
logTextHandle handle = LogAction $ liftIO . TIO.hPutStrLn handle
{-# INLINE logTextHandle #-}
{-# SPECIALIZE logTextHandle :: Handle -> LogAction IO T.Text #-}

{- | Action that prints 'Text' to file. See 'withLogStringFile' for details. -}
withLogTextFile :: MonadIO m => FilePath -> (LogAction m T.Text -> IO r) -> IO r
withLogTextFile path action = withFile path AppendMode $ action . logTextHandle
{-# INLINE withLogTextFile #-}
{-# SPECIALIZE withLogTextFile :: FilePath -> (LogAction IO T.Text -> IO r) -> IO r #-}
