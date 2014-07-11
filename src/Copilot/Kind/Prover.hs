--------------------------------------------------------------------------------

module Copilot.Kind.Prover 
  ( Cex (..)
  , Error
  , Output (..)
  , Feature (..)
  , Prover (..)
  , combine
  ) where

import Copilot.Kind.ProofScheme
import qualified Copilot.Core as Core

import Data.List (intercalate)
import Control.Applicative (liftA2, liftA)

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

-- Thank you GHC for compiling this
combine :: Prover -> Prover -> Prover
combine 
  (Prover { proverName  = proverNameL 
          , hasFeature  = hasFeatureL 
          , startProver = startProverL
          , askProver   = askProverL
          , closeProver = closeProverL 
          })
          
  (Prover { proverName  = proverNameR
          , hasFeature  = hasFeatureR
          , startProver = startProverR
          , askProver   = askProverR
          , closeProver = closeProverR
          })

 = Prover
  { proverName  = proverNameL ++ "_" ++ proverNameR
  , hasFeature  = liftA2 (||) hasFeatureL hasFeatureR
  , startProver = \spec -> do
      proverL <- startProverL spec
      proverR <- startProverR spec
      return (proverL, proverR)
      
  , askProver = \(stL, stR) assumptions toCheck ->
      liftA2 combineOutputs 
        (askProverL stL assumptions toCheck)
        (askProverR stR assumptions toCheck)
      
  , closeProver = \(stL, stR) -> do
      closeProverL stL
      closeProverR stR
  }
  
combineOutputs :: Output -> Output -> Output
combineOutputs (Error s) _               = Error ("LEFT:" ++ s)
combineOutputs  _ (Error s)              = Error ("RIGHT:" ++ s)
combineOutputs Valid Valid               = Valid
combineOutputs Valid (Invalid _)         = Error "The two provers don't agree"
combineOutputs Valid Unknown             = Valid
combineOutputs (Invalid (Just cex)) _    = (Invalid $ Just cex)
combineOutputs (Invalid _) (Invalid _)   = Invalid Nothing
combineOutputs (Invalid Nothing) Unknown = Invalid Nothing
combineOutputs Unknown Unknown           = Unknown
combineOutputs o1 o2                     = combineOutputs o2 o1
  
--------------------------------------------------------------------------------