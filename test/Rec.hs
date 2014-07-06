{-# LANGUAGE RebindableSyntax #-}

module Rec where

import Prelude ()
import Copilot.Language

spec :: Spec
spec = do
  -- check "xnull" (x == 0)
  check "nxnul" (x /= 0)

  where
    x :: Stream Word64
    x = [0] ++ x

