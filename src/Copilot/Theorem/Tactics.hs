module Copilot.Theorem.Tactics
  ( instantiate, assume, admit
  , trigArith
  , kInductZ3, inductZ3
  , kInduct, induct
  , module Z3
  ) where

import Copilot.Theorem.Prove
import qualified Copilot.Theorem.Light.Prover as SMT
import Copilot.Theorem.Light.Z3Prover as Z3
import Copilot.Theorem.Light.Backend

import Data.Word
import Control.Monad.Writer

instantiate :: Proof Universal -> Proof Existential
instantiate (Proof p) = Proof p

assume :: PropRef Universal -> Proof a
assume (PropRef p) = Proof $ tell [Assume p]

admit :: Proof a
admit = Proof $ tell [Admit]

trigArith :: Proof Universal
trigArith = SMT.onlyValidity def SMT.dReal

kInductZ3 :: Word32 -> Proof Universal
kInductZ3 k = Z3.kInduction def { Z3.nraNLSat = False, startK = k, maxK = k }

inductZ3 :: Proof Universal
inductZ3 = kInductZ3 0

kInduct :: SmtFormat a => Word32 -> Backend a -> Proof Universal
kInduct k = SMT.kInduction def { SMT.startK = k, SMT.maxK = k }

induct :: SmtFormat a => Backend a -> Proof Universal
induct = kInduct 0
