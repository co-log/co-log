{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Main (main) where

import Hedgehog (MonadGen, Property, checkSequential, discover, forAll, property, (===))
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import Colog.Core (LogAction)
import Colog.Monad (logMsg, usingLoggerT)
import Colog.Pure (PureLogger, logMessagePure, runPureLog)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


data LogAST
    = LogMsg
    | AND LogAST LogAST
    deriving stock (Show)

listLogMessages :: LogAST -> String -> [String]
listLogMessages LogMsg msg    = [msg]
listLogMessages (AND a b) msg = listLogMessages a msg ++ listLogMessages b msg

processAST :: LogAction (PureLogger String) String -> String -> [String]
processAST action msg = snd $ runPureLog $ usingLoggerT action $ logMsg msg

toLoggerAction :: LogAST -> LogAction (PureLogger String) String
toLoggerAction LogMsg    = logMessagePure
toLoggerAction (AND a b) = toLoggerAction a <> toLoggerAction b

genAST :: MonadGen m => m LogAST
genAST = Gen.recursive
    Gen.choice
    [ Gen.constant LogMsg ]
    [ Gen.subterm2 genAST genAST AND ]

prop_test_validate :: Property
prop_test_validate = property $ do
    msg <- forAll $ Gen.string (Range.constant 1 100) Gen.unicode
    ast <- forAll genAST
    processAST (toLoggerAction ast) msg === listLogMessages ast msg

prop_test_assoc :: Property
prop_test_assoc = property $ do
    msg <- forAll $ Gen.string (Range.constant 1 100) Gen.unicode
    x <- forAll genAST
    y <- forAll genAST
    z <- forAll genAST
    let ax = toLoggerAction x
    let ay = toLoggerAction y
    let az = toLoggerAction z
    processAST ((ax <> ay) <> az) msg === processAST (ax <> (ay <> az)) msg

tests :: IO Bool
tests = checkSequential $$(discover)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    tests >>= \p -> if p then exitSuccess else exitFailure
