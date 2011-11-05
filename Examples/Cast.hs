--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot hiding (even, odd)

b :: Stream Bool
b = [True] ++ not b

i :: Stream Int8
i = cast b

x :: Stream Word16
x = [0] ++ x + 1

y :: Stream Int32
y = 1 + cast x

spec :: Spec
spec = trigger "trigger" true [arg y, arg i]

main :: IO ()
main = interpret 10 [] spec
