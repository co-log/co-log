{- | The purpose of this module is to check concurrent abilities of @colog@.
-}
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, replicateM_, void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Semigroup ((<>))
import Prelude hiding (log)

import Colog (LogAction, logByteStringStderr, logByteStringStdout, (<&))


{- | Action that prints predefined message 5 times.
-}
action :: LogAction IO ByteString -> ByteString -> IO ()
action log msg = replicateM_ 5 $ log <& msg

{- | Checks @colog@ for interleaved output. Forks 10 threads, each of them runs
'action'.
-}
main :: IO ()
main = do
    forM_ [1..10 :: Int] $ \i -> do
        let log = if even i then logByteStringStdout else logByteStringStderr
        void $ forkIO $ action log $ "Logging from thread: " <> pack (show i)
    threadDelay $ 3 * (10 ^ (6 :: Int)) -- wait 3 seconds
