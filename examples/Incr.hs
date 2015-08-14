{-# LANGUAGE RebindableSyntax #-}

module Incr where

import Prelude ()
import Language.Copilot

import Copilot.Theorem
import Copilot.Theorem.Prover.Z3

spec :: Spec
spec = do
  prop "gt1" (exists $ x < 1)
  prop "neq0" (exists $ x == 0)

  where
    x :: Stream Word8
    x = [2] ++ (1 + x)

scheme prover = do
  assert prover "gt1"
  check prover "neq0"
