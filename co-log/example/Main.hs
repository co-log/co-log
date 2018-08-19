{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Colog (LogAction (..), WithLog, cmap, logMsg, usingLoggerT, withLog)

example :: WithLog env String m => m ()
example = do
    logMsg "First message..."
    logMsg "Second message..."

app :: WithLog env String m => m ()
app = do
    logMsg "Starting application..."
    withLog (cmap ("app:" ++)) example

main :: IO ()
main = usingLoggerT (LogAction $ putStrLn @String) app
