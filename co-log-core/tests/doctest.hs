module Main
       ( main
       ) where

import Test.DocTest (doctest)

main :: IO ()
main = Doctest.doctest ["-XInstanceSigs", "src"]
