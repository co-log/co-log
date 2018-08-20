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

foo :: (WithLog env String m, WithLog env Int m) => m ()
foo = do
    logMsg "String message..."
    logMsg @Int 42

main :: IO ()
main = usingLoggerT (LogAction $ putStrLn @String) app
