{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

module Main
       ( main
       ) where

import Control.Exception (onException)
import Control.Monad (replicateM_)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Max (..), (<>))
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Traversable (for)
import GHC.Stack (HasCallStack, callStack, emptyCallStack)
import System.Environment (getArgs)
import System.IO (IOMode (..), withFile)
import System.Posix.Process (getProcessID)
import System.Process.Typed (closed, proc, runProcess_, setStderr, setStdin, setStdout,
                             useHandleClose)

import Colog (pattern D, LogAction, Message, Msg (..), cmap, cmapM, defaultFieldMap, fmtMessage,
              fmtRichMessageDefault, logByteStringStderr, logByteStringStdout, logPrint,
              logStringStdout, logTextStdout, upgradeMessageAction, (<&))

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text.Encoding
import qualified Data.Text.IO as Text


-- | Named and specialized version of pair constructor for creating benchmarks.
bench :: String -> IO () -> (String, IO ())
bench = (,)

-- | List of benchmarks.
benchs :: [(String, IO ())]
benchs =
    [ bench "Prelude.putStrLn" $
        runIO putStrLn "message"

    , bench "Text.putStrLn" $
        runIO Text.putStrLn "message"

    , bench "ByteString.putStrLn" $
        runIO ByteString.putStrLn "message"

    , bench "mempty" $
        runLA mempty ("message" :: String)

    , bench "logStringStdout" $
        let la = logStringStdout
        in runLA la "message"

    , bench "logPrint" $
        let la = logPrint
        in runLA la (5 :: Int)

    , bench "logTextStdout" $
        let la = logTextStdout
        in runLA la "message"

    , bench "logByteStringStdout" $
        let la = logByteStringStdout
        in runLA la "message"

    , bench "logByteStringStderr" $
        let la = logByteStringStderr
        in runLA la "message"

    , bench "ByteString > (stdout <> stderr)" $
        let la = logByteStringStdout <> logByteStringStderr
        in runLA la "message"

    , bench "Message > format > stdout" $
        let la = cmap fmtMessage logTextStdout
        in runLA la msg

    , bench "Message > format > ByteString > stdout" $
        let la = cmap
                (Data.Text.Encoding.encodeUtf8 . fmtMessage)
                logByteStringStdout
        in runLA la msg

    , bench "Message{callstack} > format > stdout" $
        let la = cmap fmtMessage logTextStdout
        in runLA la (Msg D callStack "message")

    , bench "Message{callstack:5} > format > stdout" $
        let la = cmap fmtMessage logTextStdout
        in nest 5 $ runLA la (Msg D callStack "message")

    , bench "Message{callstack:50} > format > stdout" $
        let la = cmap fmtMessage logTextStdout
        in nest 50 $ runLA la (Msg D callStack "message")

    , bench "Message{Time,ThreadId} > format > stdout" $ do
        let richMessageAction = cmapM fmtRichMessageDefault logTextStdout
        let la = upgradeMessageAction defaultFieldMap richMessageAction
        runLA la msg
    ]
  where
    samples10K :: Int
    samples10K = 10000

    msg :: Message
    msg = Msg D emptyCallStack "message"

    -- run @LogAction IO@ over single message 10K times
    runLA :: LogAction IO a -> a -> IO ()
    runLA la a = replicateM_ samples10K (la <& a)

    -- runs IO action 10 K times
    runIO :: (a -> IO ()) -> a -> IO ()
    runIO action = replicateM_ samples10K .  action

    nest :: HasCallStack => Int -> IO a -> IO a
    nest 0 f = f
    nest n f = nest (n - 1) f `onException` pure () -- force nesting

main :: IO ()
main = getArgs >>= \case
    [] -> do
        putStrLn "Dump 10k messages (in a forked process):"
        results <- runBenchmarks benchs
        putStr $ genTable results
    name:_ -> fromMaybe (putStrLn "No benchmark with such name") $
        name `lookup` benchs

{- | Measure the running time of the process. The process allowed to dump data
to stdout or /dev/null. We measure the total running time of the process, and do
not verify that all logs were actually dumped, process should do that on it's
own.
-}
timeProcess :: String -> IO NominalDiffTime
timeProcess n = do
    t <- getCurrentTime
    pid <- getProcessID
    withFile "/dev/null" AppendMode $ \fnull1 ->
        withFile "/dev/null" AppendMode $ \fnull2 -> do
            let cfg = setStdin  closed
                    $ setStdout (useHandleClose fnull1)
                    $ setStderr (useHandleClose fnull2)
                    $ proc ("/proc/" ++ show pid ++ "/exe") [n]
            runProcess_ cfg
    t' <- getCurrentTime
    pure $ t' `diffUTCTime` t

runBenchmarks :: [(String, IO ())] -> IO [(String, NominalDiffTime)]
runBenchmarks bs = for bs $ \(name, _) -> do
    t <- timeProcess name
    pure (name, t)

{- | Function that takes list of pairs - benchmark name and result and generates
markdown table.
-}
genTable :: [(String, NominalDiffTime)] -> String
genTable rawResults = unlines rows
  where
    -- stringified and quoted results
    results :: [(String, String)]
    results = map (bimap quote (quote . fmtTime)) rawResults

    -- Takes length of string as Max monoid
    strLen :: String -> Max Int
    strLen = Max . length

    quote :: String -> String
    quote s = "`" <> s <> "`"

    nameMax, timeMax :: Int
    (nameMax, timeMax) = coerce $ foldMap (bimap strLen strLen) results

    padLeft, padRight :: Char -> Int -> String -> String
    padLeft  c limit s =      replicate (limit - length s) c ++ s
    padRight c limit s = s ++ replicate (limit - length s) c

    rows :: [String]
    rows = map toTableRow
        $ (padRight ' ' nameMax "Benchmarks", padRight ' ' timeMax "Time")
        : (':' : replicate (nameMax - 1) '-', ':' : replicate (timeMax - 1) '-')
        : map (bimap (padRight ' ' nameMax) (padRight ' ' timeMax)) results

    toTableRow :: (String, String) -> String
    toTableRow (l, r) = "| " ++ l ++ " | " ++ r ++ " |"

    -- formats time as milliseconds like this: ` 23.987ms`
    fmtTime :: NominalDiffTime -> String
    fmtTime = formatMillis . properFraction . (* 1000) . toRational
      where
        formatMillis :: (Int, Rational) -> String
        formatMillis (n, f) = concat
            [ padLeft  ' ' 3 $ show n
            , padRight '0' 4 $ fmtRat f
            , "ms"
            ]

        fmtRat :: Rational -> String
        fmtRat = take 4 . dropWhile (/= '.') . show . fromRational @Double
