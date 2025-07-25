-- Copyright 2025 NASA
-- Copyright 2019 National Institute of Aerospace / Galois, Inc.

-- This is a simple example with basic usage. It implements a simple home
-- heating system: It heats when temp gets too low, and stops when it is high
-- enough. It read temperature as a byte (range -50C to 100C) and translates
-- this to Celsius.

module Main where

import Language.Copilot
import Copilot.Visualize.Static

import Prelude hiding ((>), (<))

-- External temperature as a byte, range of -50C to 100C
temp :: Stream Word8
temp = extern "temperature" (Just [ 10, 15, 18, 19, 25, 5, 45 ])

-- Calculate temperature in Celsius.
-- We need to cast the Word8 to a Float. Note that it is an unsafeCast, as there
-- is no direct relation between Word8 and Float.
ctemp :: Stream Float
ctemp = (unsafeCast temp) * (150.0 / 255.0) - 50.0

spec = do
  -- Triggers that fire when the ctemp is too low or too high,
  -- pass the current ctemp as an argument.
  trigger "heaton"  (ctemp < 18.0) [arg ctemp]
  trigger "heatoff" (ctemp > 21.0) [arg ctemp]

-- Compile the spec and produce a LaTeX file in the current directory with a
-- Tikz drawing of the spec.
main :: IO ()
main = do
  spec' <- reify spec
  visualize 3 spec' "tikz" "."
