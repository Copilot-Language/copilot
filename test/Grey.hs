{-# LANGUAGE RebindableSyntax #-}

module Grey (spec, scheme) where

import Prelude ()
import Copilot.Language

import Copilot.Kind.ProofScheme

intCounter :: Stream Bool -> Stream Word64
intCounter reset = time
  where time = if reset 
               then 0 
               else [0] ++ if time == 3 then 0 else time + 1


greyTick :: Stream Bool -> Stream Bool
greyTick reset = a && b
  where
    a = (not reset) && ([False] ++ not b)
    b = (not reset) && ([False] ++ a)


spec :: Spec
spec = do
  prop "counterOk"   (r ==> (ic == 0))
  prop "counterNOk"  (r ==> (ic /= 0))
  prop "eqCounters"  (it /= gt)
  
  prop "vneq0" (v /= 0)
  prop "vgt1" (v > 1)
  
  observer "ok" (it == gt)
  observer "int" it
  observer "grey" gt
  observer "reset" r 

  where
    ic = intCounter r
    it = ic == 2
    gt = greyTick r
    r  = [False, False, True, False, True] ++ r
    
    v :: Stream Word64
    v = [3] ++ (1 + v)
    
scheme :: ProofScheme
scheme = proof $ do
  msg "Helo world"
  assuming ["vgt1"] $ do
    check "vneq0"
  check "vneq0"
  
  
