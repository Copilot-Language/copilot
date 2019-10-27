--------------------------------------------------------------------------------
-- Copyright © 2019 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Another small example that calculates a constant value using a recursive
-- function.

module Main where

import Language.Copilot
import Copilot.Compile.C99

spec :: Spec
spec = trigger "f" true [ arg $ mult 5 ]
  where
    mult :: Word64 -> Stream Word64
    mult 0 = 1
    mult i = constant i * mult (i-1)

main :: IO ()
main = interpret 100 spec
