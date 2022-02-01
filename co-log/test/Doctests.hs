module Doctests(tests) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

tests :: IO ()
tests = do
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
