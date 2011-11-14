--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Main (main) where

import Copilot.Core.PrettyPrint (prettyPrint)
import Copilot.Core.Random (randomSpec, randomExtVals)
import Copilot.Core.Random.Weights (Weights (..), simpleWeights)
import Copilot.Compile.C99.Test.CheckSpec (checkSpec)

import Prelude
import System.Random
import Control.Monad (when, unless)

myWeights :: Weights
myWeights =
  simpleWeights
    { maxExprDepth = 2
    , maxBuffSize  = 2
    , maxTriggers  = 1
    , maxTrigArgs  = 1
    , maxObservers = 0
    , numStreams   = 3 
    , floatFreq    = 0
    , doubleFreq   = 0 
    , divModFreq   = False
    }

-- XXX we'll make this a parameter at some point
numIterations :: Int
numIterations = 10

testRandomSpec :: IO Bool
testRandomSpec = do
  g <- newStdGen
  let spec = randomSpec myWeights g
  let env = randomExtVals numIterations spec myWeights g
  putStrLn "------------------------------------------"
  putStrLn "Specification to test:"
  putStrLn $ prettyPrint spec
  checkSpec numIterations env spec

main :: IO ()
main = do
  putStrLn "Enter the number of random specifications to test:"
  i <- readLn :: IO Int
  go i
  where 
  go 0 = putStrLn "No failures found."
  go i = do 
    b <- testRandomSpec
    when b   $ putStrLn "" >> go (i-1)
    unless b $ putStrLn "Inconsistency found!" >> return ()
                            

