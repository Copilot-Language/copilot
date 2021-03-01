{-# LANGUAGE Safe #-}

-- | Utility functions to help write proof tactics.

module Copilot.Theorem.Tactics
  ( instantiate, assume, admit
  ) where

import Copilot.Theorem.Prove

import Control.Monad.Writer

-- | Instantiate a universal proof into an existential proof.
instantiate :: Proof Universal -> Proof Existential
instantiate (Proof p) = Proof p

-- | Assume that a property, given by reference, holds.
assume :: PropRef Universal -> Proof a
assume (PropRef p) = Proof $ tell [Assume p]

admit :: Proof a
admit = Proof $ tell [Admit]
