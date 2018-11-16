{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Main where

import Prelude hiding (log)

import Control.Concurrent (threadDelay)
import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Semigroup ((<>))

import Colog (pattern D, HasLog (..), LogAction, Message (..), PureLogger, WithLog, cmap, cmapM,
              defaultFieldMap, fmtMessage, fmtRichMessageDefault, liftLogIO, log, logException,
              logInfo, logMessagePure, logMsg, logMsgs, logPrint, logStringStdout, logTextStderr,
              logTextStdout, logWarning, runPureLog, upgradeMessageAction, usingLoggerT, withLog,
              withLogTextFile, (*<), (>$), (>$<), (>*), (>*<), (>|<))

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
        exceptionL
        logInfo "Application finished..."
  where
    addApp :: Message -> Message
    addApp msg = msg { messageText = "app: " <> messageText msg }



data ExampleException = ExampleException
    deriving (Show, Exception)

exceptionL :: (WithLog env Message m) => m ()
exceptionL = logException ExampleException

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

-- Returns log action that logs given string ignoring its input.
constL :: String -> LogAction IO a
constL s = s >$ stringL

intL :: LogAction IO Int
intL = logPrint

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
-- Custom monad and logger actions of different types
----------------------------------------------------------------------------

data Env m = Env
    { envLogString :: LogAction m String
    , envLogInt    :: LogAction m Int
    }

instance HasLog (Env m) String m where
    getLogAction :: Env m -> LogAction m String
    getLogAction = envLogString

    setLogAction :: LogAction m String -> Env m -> Env m
    setLogAction newAction env = env { envLogString = newAction }

instance HasLog (Env m) Int m where
    getLogAction :: Env m -> LogAction m Int
    getLogAction = envLogInt

    setLogAction :: LogAction m Int -> Env m -> Env m
    setLogAction newAction env = env { envLogInt = newAction }

newtype FooM a = FooM
    { runFooM :: ReaderT (Env FooM) IO a
    } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env FooM))

usingFooM :: Env FooM -> FooM a -> IO a
usingFooM env = flip runReaderT env . runFooM

foo :: (WithLog env String m, WithLog env Int m) => m ()
foo = do
    logMsg ("String message..." :: String)
    logMsg @Int 42

logFoo :: IO ()
logFoo = usingFooM env foo
  where
    env :: Env FooM
    env = Env
        { envLogString = liftLogIO logStringStdout
        , envLogInt    = liftLogIO logPrint
        }

----------------------------------------------------------------------------
-- main runner
----------------------------------------------------------------------------

main :: IO ()
main = withLogTextFile "co-log/example/example.log" $ \logTextFile -> do
    let runApp :: LogAction IO Message -> IO ()
        runApp action = usingLoggerT action app

    let textAction = logTextStdout <> logTextStderr <> logTextFile

    let simpleMessageAction = cmap  fmtMessage            textAction
    let richMessageAction   = cmapM fmtRichMessageDefault textAction

    let fullMessageAction = upgradeMessageAction defaultFieldMap richMessageAction
    let semiMessageAction = upgradeMessageAction
                                (TM.delete @"threadId" defaultFieldMap)
                                richMessageAction

    runApp simpleMessageAction
    runApp fullMessageAction
    runApp semiMessageAction

    usingLoggerT carL $ logMsg $ Car "Toyota" "Corolla" (Pistons 4)

    let pureAction :: PureLogger Message ()
        pureAction = usingLoggerT logMessagePure example
    let ((), msgs) = runPureLog pureAction
    usingLoggerT simpleMessageAction $ logMsgs msgs

    logFoo
