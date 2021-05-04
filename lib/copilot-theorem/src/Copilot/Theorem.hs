--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

-- | Highly automated proof techniques are a necessary step for the widespread
-- adoption of formal methods in the software industry. Moreover, it could
-- provide a partial answer to one of its main issue which is scalability.
--
-- Copilot-theorem is a Copilot library aimed at checking automatically some
-- safety properties on Copilot programs. It includes:
--
-- * A prover producing native inputs for the Kind2 model checker.
--
-- * A What4 backend that uses SMT solvers to prove safety properties.

module Copilot.Theorem
  ( module X
  , Proof
  , PropId, PropRef
  , Universal, Existential
  ) where

import Copilot.Theorem.Tactics as X
import Copilot.Theorem.Prove

--------------------------------------------------------------------------------
