{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Control.Concurrent (threadDelay)

import Colog (pattern D, LogAction, Message (..), PureLogger, WithLog, cbind, cmap,
              defaultMessageMap, fmtMessage, fmtRichMessageDefault, log, logInfo, logMessagePure,
              logMsg, logMsgs, logStringStdout, logTextStderr, logTextStdout, logWarning,
              runPureLog, upgradeMessageAction, usingLoggerT, withLog, withLogTextFile, (*<), (>$),
              (>$<), (>*), (>*<), (>|<))

import qualified Data.TypeRepMap as TM

example :: WithLog env Message m => m ()
example = do
    log D "First message..."
    logInfo "Second message..."

app :: (WithLog env Message m, MonadIO m) => m ()
app = do
    logWarning "Starting application..."
    liftIO $ threadDelay $ 10^(6 :: Int)
    withLog (cmap addApp) $ do
        example
        logInfo "Application finished..."
  where
    addApp :: Message -> Message
    addApp msg = msg { messageText = "app: " <> messageText msg }


foo :: (WithLog env String m, WithLog env Int m) => m ()
foo = do
    logMsg ("String message..." :: String)
    logMsg @Int 42

----------------------------------------------------------------------------
-- Section with contravariant combinators example
----------------------------------------------------------------------------

data Engine = Pistons Int | Rocket

engineToEither :: Engine -> Either Int ()
engineToEither e = case e of
    Pistons i -> Left i
    Rocket    -> Right ()

data Car = Car
    { carMake   :: String
    , carModel  :: String
    , carEngine :: Engine
    }

carToTuple :: Car -> (String, (String, Engine))
carToTuple (Car make model engine) = (make, (model, engine))

stringL :: LogAction IO String
stringL = logStringStdout

-- Combinator that allows to log any showable value
showL :: Show a => LogAction IO a
showL = cmap show stringL

-- Returns log action that logs given string ignoring its input.
constL :: String -> LogAction IO a
constL s = s >$ stringL

intL :: LogAction IO Int
intL = showL

-- log actions that logs single car module
carL :: LogAction IO Car
carL = carToTuple
    >$< (constL "Logging make..." *< stringL >* constL "Finished logging make...")
    >*< (constL "Logging model.." *< stringL >* constL "Finished logging model...")
    >*< ( engineToEither
      >$< constL "Logging pistons..." *< intL
      >|< constL "Logging rocket..."
        )

----------------------------------------------------------------------------
-- main runner
----------------------------------------------------------------------------

main :: IO ()
main = withLogTextFile "co-log/example/example.log" $ \logTextFile -> do
    let runApp :: LogAction IO Message -> IO ()
        runApp action = usingLoggerT action app

    let textAction = logTextStdout <> logTextStderr <> logTextFile

    let simpleMessageAction = cmap  fmtMessage            textAction
    let richMessageAction   = cbind fmtRichMessageDefault textAction

    let fullMessageAction = upgradeMessageAction defaultMessageMap richMessageAction
    let semiMessageAction = upgradeMessageAction
                                (TM.delete @"thread-id" defaultMessageMap)
                                richMessageAction

    runApp simpleMessageAction
    runApp fullMessageAction
    runApp semiMessageAction

    usingLoggerT carL $ logMsg $ Car "Toyota" "Corolla" (Pistons 4)

    let pureAction :: PureLogger Message ()
        pureAction = usingLoggerT logMessagePure example
    let ((), msgs) = runPureLog pureAction
    usingLoggerT simpleMessageAction $ logMsgs msgs
