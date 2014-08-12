{-# LANGUAGE RebindableSyntax #-}

module Grey (spec, scheme) where

import Prelude ()
import Language.Copilot
import Copilot.Kind

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

spec :: Spec
spec = do
  prop "iResetOk"   (r ==> (ic == 0))
  prop "eqCounters" (it == gt)
  
  where
    ic = intCounter r
    it = ic == 2
    gt = greyTick r
    r  = externB "reset" Nothing
    
scheme :: ProofScheme
scheme = do
  check "iResetOk"
  check "eqCounters"
  
  
