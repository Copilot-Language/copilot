{-# LANGUAGE RebindableSyntax #-}

module Incr where

import Prelude ()
import Language.Copilot
import Copilot.Kind

spec :: Spec
spec = do
  prop "gt1" (x > 1)
  prop "neq0" (x /= 0)

  where
    x :: Stream Word8
    x = [2] ++ (1 + x)
    
scheme :: ProofScheme
scheme = do
  assert "gt1"
  check  "neq0"