module Incr where

import Prelude ()
import Copilot.Language

import Copilot.Theorem
import Copilot.Theorem.Prover.SMT (def, induction, debug, z3)

spec = do
  bounds <- prop "bounds" (forAll $ x < 255)
  theorem "gt1" (forAll $ x > 1) (assume bounds >> induct)
  theorem "neq0" (forAll $ x /= 0) (assume bounds >> induct)

  where
    x :: Stream Word8
    x = [2] ++ (1 + x)

induct :: Proof Universal
induct = induction def { debug = True } z3
