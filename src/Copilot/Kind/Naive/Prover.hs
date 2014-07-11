--------------------------------------------------------------------------------

module Copilot.Kind.Naive.Prover
  ( module Data.Default
  , Options (..)
  , naiveProver
  ) where

import Copilot.Kind.IL.Translate
import Copilot.Kind.IL 

import qualified Copilot.Core as Core
import qualified Copilot.Kind.Naive.SMT as SMT

import Control.Monad
import Data.Default

import Copilot.Kind.Prover

import qualified Data.Map as Map
import Data.Map (Map)

--------------------------------------------------------------------------------

data Options = Options 
  { kTimeout  :: Integer
  , debugMode :: Bool }
               
instance Default Options where
  def = Options 
    { kTimeout  = 100
    , debugMode = False }
    
data ProverST = ProverST
  { options  :: Options
  , spec     :: Spec }
    
naiveProver :: Options -> Prover
naiveProver options = Prover
  { proverName = "Naive"
  , hasFeature = \case
      GiveCex -> False
      HandleAssumptions -> False
  , startProver = \spec -> return $ ProverST options (translate spec)
  , askProver   = ask
  , closeProver = const $ return ()
  
  }

--------------------------------------------------------------------------------

-- | Checks the Copilot specification with k-induction

ask :: ProverST -> [PropId] -> [PropId] -> IO Output
ask 
  (ProverST opts (Spec {modelInit, modelRec, properties, sequences})) 
  assumptionsIds
  toCheckIds = do

    baseSolver <- SMT.startNewSolver "base" sequences (debugMode opts)
    stepSolver <- SMT.startNewSolver "step" sequences (debugMode opts)
  
    SMT.assume baseSolver modelInit
    SMT.assume stepSolver (modelInit ++ modelRec)
  
    res <- indStep 0 baseSolver stepSolver
    mapM_ SMT.exit [baseSolver, stepSolver]
    return res
    
    where
  
      at = evalAt
      assumptions = selectProps assumptionsIds properties
      toCheck     = selectProps toCheckIds     properties
  
      indStep k baseSolver stepSolver
        | k > kTimeout opts = return Unknown
        | otherwise = do
  
          let base' = map (at $ Fixed k) modelRec
              step' = map (at $ _n_plus (k + 1)) modelRec
                      ++ map (at $ _n_plus k) toCheck
                    
              baseInv  = map (at $ Fixed k) toCheck
              nextInv  = map (at $ _n_plus (k + 1)) toCheck
  
          SMT.assume baseSolver base'
          SMT.assume stepSolver step'
        
          SMT.entailed baseSolver baseInv >>= \case
            SMT.Sat _   -> return $ Invalid Nothing
            SMT.Unknown -> return $ Unknown
            SMT.Unsat   -> do
            
                SMT.entailed stepSolver nextInv >>= \case
                  SMT.Sat _   -> indStep (k + 1) baseSolver stepSolver
                  SMT.Unsat   -> return Valid
                  SMT.Unknown -> return Unknown


selectProps :: [PropId] -> Map PropId Constraint -> [Constraint]
selectProps propIds properties = 
  [c | (id, c) <- Map.toList properties, id `elem` propIds]

--------------------------------------------------------------------------------
  