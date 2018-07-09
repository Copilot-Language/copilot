module Main where

import Language.Copilot
import Copilot.Compile.C

import Prelude hiding ((++), drop, div)

s0 :: Stream Int32
s0 = [1,2,3] ++ s1

s1 :: Stream Int32
s1 = [4,5] ++ s1

s2 = ((s0 + 5) `div` 3) * s1 + (10 `div` 2) - s0 + ex

ex :: Stream Int32
ex = extern "exvar" Nothing

ex2 :: Stream Bool
ex2 = extern "exbool" Nothing

spec = do
  trigger "fbasic" true [arg s2]
  trigger "fbool" true [arg ex2]

main = do
  reify spec >>= compile (defaultParams { prefix = Just "large" })
