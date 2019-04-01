--------------------------------------------------------------------------------
-- Copyright © 2019 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- This is a simple example for arrays. As a program, it does not make much
-- sense, however it shows of the features of arrays nicely.

-- Enable compiler extension for type-level data, necesary for the array length.
{-# LANGUAGE DataKinds #-}

module Array where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((++), (>))

-- Lets define an array of length 2.
-- Make the buffer of the streams 3 elements long.
arr :: Stream (Array 2 Bool)
arr = [ array [True, False]
      , array [True, True]
      , array [False, False]] ++ arr

-- Refer to an external array.
exarr :: Stream (Array 3 Int8)
exarr = extern "exarr" Nothing

spec = do
  -- A trigger that fires 'func' when the first element of 'arr' is True.
  -- It passes the current value of exarr as an argument.
  -- The prototype of 'func' would be:
  -- void func (int8_t arg[3]);
  trigger "func" (arr .!! 0) [arg exarr]

-- Compile the spec
main = reify spec >>= compile "array"
