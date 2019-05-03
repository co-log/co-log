module Main where

import Prelude hiding (log)

import Polysemy (Member, Sem, runM)

import Colog.Core.IO (logStringStdout)
import Colog.Polysemy (Log, log, runLogAction)


example :: Member (Log String) r => Sem r ()
example = do
    log @String "First message..."
    log @String "Second message..."

main :: IO ()
main = runM $ runLogAction @IO logStringStdout example
