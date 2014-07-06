{-# LANGUAGE RebindableSyntax #-}

module Counter where

import Prelude ()
import Copilot.Language

counter1 :: Spec
counter1 = 
  trigger "f" (x == 2) []
  where
    x :: Stream Word64
    x = [0] ++ if x == 3 then 0 else x + 1

counter2 :: Spec
counter2 = 
  trigger "f" (x == 4) []
  where
    x :: Stream Word64
    x = [1] ++ if x == 5 then 1 else x + 2

spec = counter1
