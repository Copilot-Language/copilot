--------------------------------------------------------------------------------

module Copilot.Kind.Light.Prover
  ( module Data.Default
  , Options (..)
  , lightProver
  ) where

import Copilot.Kind.IL.Translate
import Copilot.Kind.IL 

import qualified Copilot.Core as Core
import qualified Copilot.Kind.Light.SMT as SMT

import Control.Monad
import Data.Default

import Copilot.Kind.Prover

import qualified Data.Map as Map
import Data.Map (Map)

--------------------------------------------------------------------------------

data Options = Options 
  { kTimeout  :: Integer
  , onlyBmc   :: Bool
  , debugMode :: Bool } 
               
instance Default Options where
  def = Options 
    { kTimeout  = 100
    , debugMode = False 
    , onlyBmc   = False }
    
data ProverST = ProverST
  { options  :: Options
  , spec     :: Spec }
    
lightProver :: Options -> Prover
lightProver options = Prover
  { proverName = "Light"
  , hasFeature = \case
      GiveCex -> False
      HandleAssumptions -> True
  , startProver = \spec -> return $ ProverST options (translate spec)
  , askProver   = ask
  , closeProver = const $ return ()
  }

--------------------------------------------------------------------------------

-- | Checks the Copilot specification with k-induction

ask :: ProverST -> [PropId] -> PropId -> IO Output
ask 
  (ProverST opts (Spec {modelInit, modelRec, properties, sequences, unintFuns})) 
  assumptionsIds
  toCheckId = do

    baseSolver <- SMT.startNewSolver "base" sequences unintFuns (debugMode opts)
    stepSolver <- SMT.startNewSolver "step" sequences unintFuns (debugMode opts)
  
    SMT.assume baseSolver modelInit'
    SMT.assume stepSolver modelRec'
  
    res <- indStep 0 baseSolver stepSolver
    mapM_ SMT.exit [baseSolver, stepSolver]
    return res
    
    where
  
      at = evalAt
      assumptions = selectProps assumptionsIds properties
      toCheck     = selectProps [toCheckId]    properties
      
      modelInit'  = modelInit ++ map (at $ Fixed 0) assumptions
      modelRec'   = modelRec  ++ assumptions
  
      indStep k baseSolver stepSolver
        | k > kTimeout opts = 
          let msg = "after " ++ show (kTimeout opts) ++ " iterations"
          in return (Output Unknown [msg])
        | otherwise = do
  
          let base' = map (at $ Fixed k) modelRec'
              step' = map (at $ _n_plus (k + 1)) modelRec'
                      ++ map (at $ _n_plus k) toCheck
                    
              baseInv  = map (at $ Fixed k) toCheck
              nextInv  = map (at $ _n_plus (k + 1)) toCheck
  
          SMT.assume baseSolver base'
          SMT.assume stepSolver step'
        
          SMT.entailed baseSolver baseInv >>= \case
            SMT.Sat     -> return $ Output (Invalid Nothing) []
            SMT.Unknown -> return $ Output Unknown ["undecidable"]
            SMT.Unsat   -> do
               
              if onlyBmc opts
                then indStep (k + 1) baseSolver stepSolver
                else
            
                  SMT.entailed stepSolver nextInv >>= \case
                    SMT.Sat     -> indStep (k + 1) baseSolver stepSolver
                    SMT.Unsat   -> return $ Output Valid []
                    SMT.Unknown -> return $ Output Unknown ["undecidable"]


selectProps :: [PropId] -> Map PropId Constraint -> [Constraint]
selectProps propIds properties = 
  [c | (id, c) <- Map.toList properties, id `elem` propIds]

--------------------------------------------------------------------------------
