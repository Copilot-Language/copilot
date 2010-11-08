{-# LANGUAGE FlexibleContexts #-}

module Language.Copilot.Examples.LTLExamples where

import Prelude (replicate, Int, replicate)
import qualified Prelude as P

import Language.Copilot
import Language.Copilot.Libs.Indexes
import Language.Copilot.Libs.LTL
import Language.Copilot.Variables

----------------
-- LTL tests ---
----------------

testing, output :: Var
testing = "testing"
output = "output"

-- Can be tested with various values of val.  Use the interpreter to see the
-- outputs.

tSoonest :: Int -> Streams
tSoonest val = do 
  testing .= replicate (val+2) False ++ true
  output  .= soonest val (var testing)

tLatest :: Int -> Streams
tLatest val = do 
  testing .= replicate (val-2) False ++ true
  output  .= latest val (var testing)

tAlways :: Int -> Int -> Streams
tAlways i1 i2 = do 
  testing .=    (replicate i1 True P.++ [False]) ++ true
  output  `ltl` always i2 (varB testing)

tNext :: Int -> Streams
tNext i1 = do 
  testing .=    (replicate i1 False P.++ [True]) ++ false
  output  `ltl` next (varB testing)

tFuture :: Int -> Int -> Streams
tFuture i1 i2 = do 
  testing .=    (replicate i1 False P.++ [True]) ++ varB testing
  output  `ltl` eventually i2 (varB testing)

tUntil :: Int -> Int -> Int -> Streams
tUntil i1 i2 i3 = do 
  "t1"   .=    replicate i1 False ++ true
  "t0"   .=    replicate i2 True ++ false
  output `ltl` until i3 (varB "t0") (varB "t1")

tRelease0 :: Int -> Streams
tRelease0 val = output `ltl` release val false true

tRelease1 :: Int -> Int -> Streams
tRelease1 i1 i2 = do 
  "t1"   .=    replicate i1 True ++ varB c
  c      .=    [False] ++ not (var c)
  "t0"   .=    true
  output `ltl` release i2 (varB "t0") (varB "t1")
          
testRules :: Streams
testRules = do
    "v1" .=    (not true) || Var "v2" 
    "v2" .=    [True, False] ++ [True] ++ Var "v3" < extI8 "v4" 5 
    "v3" .=    0 + drop 3 (int8 6) 
    "v4" `ltl` always 5 (Var "v1") 

