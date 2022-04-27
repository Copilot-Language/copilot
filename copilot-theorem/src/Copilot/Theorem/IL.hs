{-# LANGUAGE Safe #-}

-- | Each prover first translates the Copilot specification into an
-- intermediate representation best suited for model checking.
--
-- This module and the ones in the same namespace implement the IL format. A
-- Copilot program is translated into a list of quantifier-free equations over
-- integer sequences, implicitly universally quantified by a free variable n.
-- Each sequence roughly corresponds to a stream.

module Copilot.Theorem.IL (module X) where

import Copilot.Theorem.IL.Spec as X
import Copilot.Theorem.IL.Translate as X
import Copilot.Theorem.IL.Transform as X
import Copilot.Theorem.IL.PrettyPrint as X
