{-# LANGUAGE FlexibleContexts #-}

module Language.Copilot.Examples.LTLExamples where

import Prelude (replicate, Int, replicate)
import qualified Prelude as P

import Language.Copilot
import Language.Copilot.Libs.Indexes
import Language.Copilot.Libs.LTL

----------------
-- LTL tests ---
----------------

-- Can be tested with various values of val.  Use the interpreter to see the
-- outputs.

testing = varB "testing"
output = varB "output"

tSoonest :: Int -> Streams
tSoonest val = do 
  testing .= replicate (val+2) False ++ true
  let out = varI16 "out"
  out     .= soonest val testing

tLatest :: Int -> Streams
tLatest val = do 
  testing .= replicate (val-2) False ++ true
  let out = varI16 "out"
  out     .= latest val testing

tAlways :: Int -> Int -> Streams
tAlways i1 i2 = do 
  testing .=    (replicate i1 True P.++ [False]) ++ true
  output  `ltl` always i2 testing

tNext :: Int -> Streams
tNext i1 = do 
  testing .=    (replicate i1 False P.++ [True]) ++ false
  output  `ltl` next testing

tFuture :: Int -> Int -> Streams
tFuture i1 i2 = do 
  testing .=    (replicate i1 False P.++ [True]) ++ testing
  output  `ltl` eventually i2 testing


c, t0, t1 :: Spec Bool
t0 = varB "t0"
t1 = varB "t1"
c = varB "c"

tUntil :: Int -> Int -> Int -> Streams
tUntil i1 i2 i3 = do 
  t1   .=    replicate i1 False ++ true
  t0   .=    replicate i2 True ++ false
  output `ltl` until i3 t0 t1

tRelease0 :: Int -> Streams
tRelease0 val = output `ltl` release val false true

tRelease1 :: Int -> Int -> Streams
tRelease1 i1 i2 = do 
  t1   .=    replicate i1 True ++ c
  c    .=    [False] ++ not c
  t0   .=    true
  output `ltl` release i2 t0 t1
          
testRules :: Streams
testRules = do
  let v1 = varB "v1"
  let v2 = varB "v2"
  let v3 = varI16 "v3"
  let v4 = varB "v4"
  let ext = extI8 "ext" 5 
  v1 .=    (not true) || v2
  v2 .=    [True, False] ++ [True] ++ v3 < cast ext
  v3 .=    0 + drop 3 6
  v4 `ltl` always 5 v1

