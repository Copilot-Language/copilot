module Copilot.Theorem.Tactics
  ( instantiate, assume, admit
  , trigArith
  , module Z3
  ) where

import Copilot.Theorem.Prove
import qualified Copilot.Theorem.Light.Prover as SMT
import Copilot.Theorem.Light.Z3Prover as Z3

import Data.Word
import Control.Monad.Writer

instantiate :: Proof Universal -> Proof Existential
instantiate (Proof p) = Proof p

assume :: PropRef a -> Proof a
assume (PropRef p) = Proof $ tell [Assume p]

admit :: Proof a
admit = Proof $ tell [Admit]

trigArith :: Proof Universal
trigArith = SMT.onlyValidity def SMT.dReal

