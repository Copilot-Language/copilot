--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RebindableSyntax #-}

module Main (main) where

import Copilot.Core.PrettyPrint (prettyPrint)
import Copilot.Core.Random (randomSpec)
import Copilot.Core.Random.Weights (Weights (..), simpleWeights)
import Copilot.Compile.C99.Test.CheckSpec (checkSpec)
import Prelude
import System.Random

myWeights :: Weights
myWeights =
  simpleWeights
    { maxExprDepth = 2
    , maxBuffSize  = 2
    , maxTriggers  = 1
    , maxTrigArgs  = 1
    , maxObservers = 0
    , numStreams   = 3 }

testRandomSpec :: IO Bool
testRandomSpec = do
  g <- newStdGen
  let spec = randomSpec myWeights g
  putStrLn $ prettyPrint spec
  checkSpec 10 spec

main :: IO ()
main = do
  v <- testRandomSpec
  print v
  return ()
