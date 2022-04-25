{-# LANGUAGE Safe #-}

-- | Each prover first translates the Copilot specification into an
-- intermediate representation best suited for model checking.
--
-- This module and the ones in the same namespace implement the TransSys
-- format. A Copilot program is /flattened/ and translated into a /state/
-- /transition system/.  In order to keep some structure in this
-- representation, the variables of this system are grouped by /nodes/, each
-- node exporting and importing variables. The /Kind2 prover/ uses this format,
-- which can be easily translated into the native format.
module Copilot.Theorem.TransSys (module X) where

import Copilot.Theorem.TransSys.Spec as X
import Copilot.Theorem.TransSys.PrettyPrint as X
import Copilot.Theorem.TransSys.Translate as X
import Copilot.Theorem.TransSys.Transform as X
