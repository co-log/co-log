module Main
       ( main
       ) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-XInstanceSigs", "src"]