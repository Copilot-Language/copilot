{-# LANGUAGE RebindableSyntax #-}

module Incr where

import Prelude ()
import Copilot.Language

spec :: Spec
spec = 
  trigger "f" (x <= 0) []

  where
    x :: Stream Word64
    x = [1] ++ (1 + x)
