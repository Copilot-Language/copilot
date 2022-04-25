-- Copyright © 2019 National Institute of Aerospace / Galois, Inc.

-- | Example showing usage of clocks to generate periodically recurring truth
-- values.

module Main where

import Language.Copilot
import Copilot.Library.Clocks

-- | We need to force a type for the argument of `period`.
p :: Word8
p = 5

-- | Both have the same period, but a different phase.
clkStream :: Stream Bool
clkStream  = clk (period p) (phase 0)

clkStream' :: Stream Bool
clkStream' = clk (period p) (phase 2)

spec :: Spec
spec = do
  observer "clk"  clkStream
  observer "clk'" clkStream'

main :: IO ()
main = interpret 30 spec
