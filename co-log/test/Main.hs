module Main(main) where

import qualified Doctests as D (tests)
import qualified Property as P (tests)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    D.tests
    P.tests >>= \p -> if p then exitSuccess else exitFailure
