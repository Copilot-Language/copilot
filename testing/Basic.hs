module Main where

import Language.Copilot
import Copilot.Compile.C

import Prelude hiding ((++), drop)

s0 :: Stream Int32
s0 = [1,2,3] ++ s1

s1 :: Stream Int32
s1 = [4,5] ++ s1

guard :: Stream Bool
guard = [True, True, False] ++ guard

spec = do
  trigger "fbasic" true [arg s0]

main = do reify spec >>= compile defaultParams
