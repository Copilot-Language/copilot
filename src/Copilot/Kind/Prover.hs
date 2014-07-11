--------------------------------------------------------------------------------

module Copilot.Kind.Prover 
  ( Cex (..)
  , Error
  , Output (..)
  , Feature (..)
  , Prover (..)
  ) where

import Copilot.Kind.ProofScheme
import qualified Copilot.Core as Core

--------------------------------------------------------------------------------

data Cex = Cex

type Error = String

data Output 
  = Valid
  | Invalid (Maybe Cex)
  | Unknown
  | Error Error
  
data Feature = GiveCex | HandleAssumptions
  
data Prover = forall r . Prover 
  { proverName     :: String
  , hasFeature     :: Feature -> Bool
  , startProver    :: Core.Spec -> IO r
  , askProver      :: r -> [PropId] -> [PropId] -> IO Output 
  , closeProver    :: r -> IO ()
  }
  
--------------------------------------------------------------------------------