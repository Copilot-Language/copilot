{-# LANGUAGE RebindableSyntax #-}

module Grey where

import Prelude ()
import Copilot.Language

import Copilot.Theorem
import Copilot.Theorem.Prover.Z3

intCounter :: Stream Bool -> Stream Word64
intCounter reset = time
  where
    time = if reset then 0
           else [0] ++ if time == 3 then 0 else time + 1


greyTick :: Stream Bool -> Stream Bool
greyTick reset = a && b
  where
    a = (not reset) && ([False] ++ not b)
    b = (not reset) && ([False] ++ a)

spec = do
  theorem "iResetOk"   (forall $ r ==> (ic == 0)) induct
  theorem "eqCounters" (forall $ it == gt) $ kinduct 3

  where
    ic = intCounter r
    it = ic == 2
    gt = greyTick r
    r  = extern "reset" Nothing

induct :: Proof Universal
induct = induction def { nraNLSat = False, debug = False }

kinduct :: Word32 -> Proof Universal
kinduct k = kInduction def { nraNLSat = False, startK = k, maxK = k, debug = False }
