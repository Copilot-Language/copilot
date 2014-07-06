{-# LANGUAGE RebindableSyntax #-}

module Grey where

import Prelude ()
import Copilot.Language


{- Copilot Core representation :

   (Word64) s0 = [1,1,1,0,1,0,0] ++ s0
   (Word64) s1 = [0] ++
                 (if ((if (s0 == 1) then 1 else s1) == 3)
                     then 0
                     else ((if (s0 == 1) then 1 else s1) + 1))
-}

intCounter :: Stream Bool -> Stream Word64
intCounter reset = time
  where time = if reset then 0 else
                 [0] ++ if time == 3 then 0 else time + 1


greyTick :: Stream Bool -> Stream Bool
greyTick reset = a && b
  where
    a = (not reset) && ([False] ++ b)
    b = (not reset) && ([False] ++ a)

spec :: Spec
spec = do
  check  "counterOk"   (r && ic == 0)
  check  "eqCounters"  (it == gt)
  observer "ok"           (it == gt)
  observer "int"   it
  observer "grey"  gt
  observer "reset" r

  where
    ic = intCounter r
    it = ic == 2
    gt = greyTick r
    r  = [False, False, True, False, True] ++ r
