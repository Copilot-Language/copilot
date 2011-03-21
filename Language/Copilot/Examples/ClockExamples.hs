-- | Clocks library tests.

module Language.Copilot.Examples.ClockExamples where

import Language.Copilot.Libs.Clocks
import Language.Copilot

clkTest :: Streams
clkTest = do
  let x = varB "x"
  let y = varB "y"
  x `clock` (period 3, phase 1)
  y `clock` (period 4, phase 3)

test = interpret clkTest 10 baseOpts
