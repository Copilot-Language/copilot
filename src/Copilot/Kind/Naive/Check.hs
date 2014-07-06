--------------------------------------------------------------------------------

module Copilot.Kind.Naive.Check
  ( module Data.Default
  , check
  ) where

import Copilot.Kind.IL.Translate
import Copilot.Kind.IL 

import qualified Copilot.Core as Core
import qualified Copilot.Kind.Naive.SMT as SMT

import Control.Monad
import Data.Default

--------------------------------------------------------------------------------

data Options = Options { kTimeout :: Integer
                       , debugMode :: Bool }
               
instance Default Options where
  def = Options { kTimeout  = 100
                , debugMode = True }

type Trace = String

data Result = QED
            | Incorrect
            | Unknown
            | Timeout
            | Error String
              
            deriving (Show)

--------------------------------------------------------------------------------

-- | Checks the Copilot specification with k-induction

check :: Options -> Core.Spec -> IO Result
check opts (translate -> Spec {modelInit, modelRec, invariants, sequences}) = do

  baseSolver <- SMT.startNewSolver "base" sequences True
  stepSolver <- SMT.startNewSolver "step" sequences False -- (debugMode opts)

  SMT.assume baseSolver modelInit
  SMT.assume stepSolver (modelInit ++ modelRec)

  res <- indStep 0 baseSolver stepSolver
  mapM_ SMT.exit [baseSolver, stepSolver]
  return res
  
  where

    at = evalAt

    indStep k baseSolver stepSolver
      | k > kTimeout opts = return Timeout
      | otherwise = do

        let base' = map (at $ Fixed k) modelRec
            step' = map (at $ _n_plus (k + 1)) modelRec
                    ++ map (at $ _n_plus k) invariants
                  
            baseInv  = map (at $ Fixed k) invariants
            nextInv  = map (at $ _n_plus (k + 1)) invariants

        SMT.assume baseSolver base'
        SMT.assume stepSolver step'
      
        SMT.entailed baseSolver baseInv >>= \case
          SMT.Sat _   -> return Incorrect
          SMT.Unknown -> return Unknown
          SMT.Unsat   -> do
          
              SMT.entailed stepSolver nextInv >>= \case
                SMT.Sat _   -> indStep (k + 1) baseSolver stepSolver
                SMT.Unsat   -> return QED
                SMT.Unknown -> return Unknown
