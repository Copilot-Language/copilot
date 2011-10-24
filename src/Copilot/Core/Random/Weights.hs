--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Core.Random.Weights
  ( Depth
  , Weights (..)
  , simpleWeights ) where

type Depth = Int

data Weights = Weights
  { maxExprDepth :: Int
  , maxBuffSize  :: Int
  , maxTriggers  :: Int
  , maxTrigArgs  :: Int
  , maxObservers :: Int
  , numStreams   :: Int
  -- Expression frequencies:
  , constFreq    :: Int
  , drop0Freq    :: Int
  , dropFreq     :: Int
  , externFreq   :: Int
  , op1Freq      :: Int
  , op2Freq      :: Int
  , op3Freq      :: Int
  -- Type frequencies:
  , boolFreq     :: Int
  , int8Freq     :: Int
  , int16Freq    :: Int
  , int32Freq    :: Int
  , int64Freq    :: Int
  , word8Freq    :: Int
  , word16Freq   :: Int
  , word32Freq   :: Int
  , word64Freq   :: Int
  , floatFreq    :: Int
  , doubleFreq   :: Int 
  , divModFreq   :: Bool }

simpleWeights :: Weights
simpleWeights = Weights
  { maxExprDepth = 10
  , maxBuffSize  = 8
  , maxTriggers  = 5
  , maxTrigArgs  = 5
  , maxObservers = 8
  , numStreams   = 10
  -- Expression frequencies:
  , constFreq    = 1
  , drop0Freq    = 1
  , dropFreq     = 1
  , externFreq   = 1
  , op1Freq      = 1
  , op2Freq      = 1
  , op3Freq      = 1
  -- Type frequencies:
  , boolFreq     = 1
  , int8Freq     = 1
  , int16Freq    = 1
  , int32Freq    = 1
  , int64Freq    = 1
  , word8Freq    = 1
  , word16Freq   = 1
  , word32Freq   = 1
  , word64Freq   = 1
  , floatFreq    = 1
  , doubleFreq   = 1 
  , divModFreq   = True }
