-- Copyright © 2019 National Institute of Aerospace / Galois, Inc.

-- | Example showing an implementation of a resettable counter.

{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import Copilot.Compile.C99

-- A resettable counter
counter :: Stream Bool -> Stream Bool -> Stream Int32
counter inc reset = cnt
  where
    cnt = if reset then 0
          else if inc then z + 1
               else z
    z = [0] ++ cnt

-- Counter that resets when it reaches 256
bytecounter :: Stream Int32
bytecounter = counter true reset
  where
    reset = counter true false `mod` 256 == 0

spec :: Spec
spec = trigger "counter" true [arg $ bytecounter]

main :: IO ()
-- main = interpret 1280 spec
main = reify spec >>= compile "counter"
