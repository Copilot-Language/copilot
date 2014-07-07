{-# LANGUAGE RebindableSyntax #-}

module Bug where

import Prelude ()
import Copilot.Language

--
--intCounter :: Stream Bool -> Stream Word64
--intCounter reset = time
--  where time = if reset then 0 else
--                 [0] ++ if time == 3 then 0 else time + 1


greyTick :: Stream Bool -> Stream Bool
greyTick reset = a && b
  where
    a = (not reset) && ([False] ++ not b)
    b = (not reset) && ([False] ++ a)

spec :: Spec
spec = do
--  check "counterOk"    (r ==> (ic == 0))
  --check "eqCounters"   (it == gt)     
  check "easy" (r ==> not gt)
  where
    --ic = intCounter r
    --it = ic == 2
    gt = greyTick r
    -- r  = [False, False, True, False, True] ++ r
    r  = [False] ++ r
    