module Main(main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
    sourceFiles <- glob "src/**/*.hs"
    doctest
        $ "-XConstraintKinds"
        : "-XDerivingStrategies"
        : "-XDeriveGeneric"
        : "-XGeneralizedNewtypeDeriving"
        : "-XLambdaCase"
        : "-XOverloadedStrings"
        : "-XRecordWildCards"
        : "-XScopedTypeVariables"
        : "-XStandaloneDeriving"
        : "-XTupleSections"
        : "-XTypeApplications"
        : "-XViewPatterns"
        : sourceFiles
