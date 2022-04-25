-- Copyright © 2019 National Institute of Aerospace / Galois, Inc.

-- | Examples of casting types.

{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot

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
main = interpret 30 spec
