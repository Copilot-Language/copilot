{-# LANGUAGE Safe #-}

module Copilot.Theorem.Tactics
  ( instantiate, assume, admit
  ) where

import Copilot.Theorem.Prove

import Control.Monad.Writer

instantiate :: Proof Universal -> Proof Existential
instantiate (Proof p) = Proof p

assume :: PropRef Universal -> Proof a
assume (PropRef p) = Proof $ tell [Assume p]

admit :: Proof a
admit = Proof $ tell [Admit]
