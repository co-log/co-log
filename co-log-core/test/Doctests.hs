module Main
       ( main
       ) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
    sourceFiles <- glob "co-log-core/src/**/*.hs"
    doctest
        $ "-XInstanceSigs"
        : "-XScopedTypeVariables"
        : "-XViewPatterns"
        : sourceFiles
