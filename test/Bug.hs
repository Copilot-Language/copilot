{-# LANGUAGE RebindableSyntax #-}

module Bug where

import Prelude ()
import Copilot.Language


intCounter :: Stream Bool -> Stream Word64
intCounter reset = time
  where time = if reset then 0 else
                 [0] ++ if time == 3 then 0 else time + 1


greyTick :: Stream Bool -> Stream Bool
greyTick reset = a && b
  where
    a = (not reset) && ([False] ++ not b)
    b = (not reset) && ([False] ++ a)

spec :: Spec
spec = do
  trigger  "counterOk"    (r && ic /= 0) []
  trigger  "eqCounters"   (it /= gt)     []
  observer "int"   it
  observer "grey"  gt
  observer "reset" r

  where
    ic = intCounter r
    it = ic == 2
    gt = greyTick r
    r  = [False, False, True, False, True] ++ r
