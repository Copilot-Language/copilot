-- | Clocks library tests.

module Language.Copilot.Examples.ClockExamples where

import qualified Prelude as P
import Language.Copilot.Libs.Clocks
import Language.Copilot

clkTest :: Streams
clkTest = do
  let x = varB "x"
  let y = varB "y"
  let a = varW8 "a"
  let b = varW8 "b"
  x `clk` (period 3, phase 1)
  y `clk` (period 4, phase 3)
  a .= [0] ++ mux x (a + 2) a
  b .= [1] ++ mux y (b + 2) b
  y `clk` (period 4, phase 3)
