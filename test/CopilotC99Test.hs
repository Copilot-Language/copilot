--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

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
    , numStreams   = 3 
    , floatFreq    = 0
    , doubleFreq   = 0 
    }

testRandomSpec :: IO Bool
testRandomSpec = do
  g <- newStdGen
  let spec = randomSpec myWeights g
  putStrLn "------------------------------------------"
  putStrLn "Specification to test:"
  putStrLn $ prettyPrint spec
  checkSpec 10 spec

main :: IO ()
main = do
  putStrLn "Enter the number of random specifications to test:"
  i <- readLn :: IO Int
  go i
  where go 0 = putStrLn "No failures found."
        go i = do b <- testRandomSpec
                  if b then do putStrLn "" 
                               go (i-1)
                    else do putStrLn "Inconsistency found!" 
                            return ()

