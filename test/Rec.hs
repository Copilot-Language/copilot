{-# LANGUAGE RebindableSyntax #-}

module Rec where

import Prelude ()
import Copilot.Language

spec :: Spec
spec = do
  prop "Ok" ((z == 3) ==> (x == 0))

  where
    x :: Stream Word64
    x = [0] ++ y
    y :: Stream Word64
    y = [1] ++ x
    z ::  Stream Word64
    z = [3, 4] ++ z

