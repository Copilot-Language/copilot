--------------------------------------------------------------------------------

module Copilot.Kind.DReal.Prover
  ( module Data.Default
  , Options (..)
  , dRealProver
  ) where

import Copilot.Kind.IL.Translate
import Copilot.Kind.IL

import qualified Copilot.Core as Core
import qualified Copilot.Kind.DReal.SMT as SMT

import Control.Monad
import Data.Default

import Copilot.Kind.Prover

import Data.Map (Map, toList)
import Data.List (intersectBy)
import Data.Function (on)
import Data.Set (union, (\\), fromList, elems)

--------------------------------------------------------------------------------

data Options = Options
  { -- The maximum number of steps of the k-induction algorithm the prover runs
    -- before giving up.
    kTimeout  :: Integer

    -- If `onlyBmc` is set to `True`, the prover will only search for
    -- counterexamples and won't try to prove the properties discharged to it.
  , onlyBmc   :: Bool

    -- If `*Debug` is set to `True`, the SMTLib queries produced by the
    -- prover are displayed in the standard output.
  , baseDebug :: Bool
  , stepDebug :: Bool
  }

instance Default Options where
  def = Options
    { kTimeout  = 100
    , baseDebug = False
    , stepDebug = False
    , onlyBmc   = False
    }

data ProverST = ProverST
  { options  :: Options
  , spec     :: Spec
  }

dRealProver :: Options -> Prover
dRealProver options = Prover
  { proverName = "dReal"
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
  (ProverST opts (Spec {modelInit, modelRec, properties, sequences, vars}))
  assumptionsIds
  toCheckId = do

    baseSolver <- SMT.startNewSolver "base" sequences vars (baseDebug opts)
    stepSolver <- SMT.startNewSolver "step" sequences vars (stepDebug opts)

    let initBaseVars = fromList $ getVars modelInit'
    SMT.declVars baseSolver $ elems initBaseVars
    SMT.assume baseSolver modelInit'

    let initStepVars = fromList $ getVars modelRec'
    SMT.declVars stepSolver $ elems initStepVars
    SMT.assume stepSolver modelRec'

    res <- indStep 0 baseSolver stepSolver initBaseVars initStepVars
    mapM_ SMT.exit [baseSolver, stepSolver]
    return res

    where
      at = evalAt
      assumptions = selectProps assumptionsIds properties
      toCheck     = selectProps [toCheckId]    properties

      modelInit'  = modelInit ++ map (at $ Fixed 0) assumptions
      modelRec'   = modelRec  ++ assumptions

      indStep k baseSolver stepSolver baseVars stepVars
        | k > kTimeout opts =
          let msg = "after " ++ show (kTimeout opts) ++ " iterations"
          in return (Output Unknown [msg])
        | otherwise = do

          let base' = map (at $ Fixed k) modelRec'
              step' = map (at $ _n_plus (k + 1)) modelRec'
                      ++ map (at $ _n_plus k) toCheck

              baseInv  = map (at $ Fixed k) toCheck
              nextInv  = map (at $ _n_plus (k + 1)) toCheck

          let baseVarsItr = fromList $ getVars base' ++ getVars baseInv
              baseVars'   = baseVars `union` (fromList $ getVars base' ++ getVars baseInv)
          SMT.declVars baseSolver $ elems $ baseVarsItr \\ baseVars
          SMT.assume baseSolver base'

          let stepVarsItr = fromList $ getVars step' ++ getVars nextInv
              stepVars'   = stepVars `union` (fromList $ getVars step' ++ getVars nextInv)
          SMT.declVars stepSolver $ elems $ stepVarsItr \\ stepVars
          SMT.assume stepSolver step'

          SMT.entailed baseSolver baseInv >>= \case
            SMT.Sat     -> return $ Output (Invalid Nothing) []
            SMT.Unknown -> return $ Output Unknown ["undecidable"]
            SMT.Unsat   -> do
              if onlyBmc opts
                then indStep (k + 1) baseSolver stepSolver baseVars' stepVars'
                else
                  SMT.entailed stepSolver nextInv >>= \case
                    SMT.Sat     -> indStep (k + 1) baseSolver stepSolver baseVars' stepVars'
                    SMT.Unsat   -> return $ Output Valid []
                    SMT.Unknown -> return $ Output Unknown ["undecidable"]

selectProps :: [PropId] -> Map PropId Constraint -> [Constraint]
selectProps propIds properties =
  [c | (id, c) <- toList properties, id `elem` propIds]

--------------------------------------------------------------------------------
