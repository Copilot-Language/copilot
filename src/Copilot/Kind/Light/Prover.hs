--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase, NamedFieldPuns, FlexibleInstances, RankNTypes, GADTs #-}

module Copilot.Kind.Light.Prover
  ( module Data.Default
  , Options (..)
  , kInduction
  , yices, dReal, altErgo, metit
  , Backend, SmtFormat
  ) where

import Copilot.Kind.IL.Translate
import Copilot.Kind.IL

import System.IO (hClose)

import Copilot.Kind.Light.Backend
import qualified Copilot.Kind.Light.SMT as SMT

import Copilot.Kind.Light.SMTLib (SmtLib)
import Copilot.Kind.Light.TPTP (Tptp)
import qualified Copilot.Kind.Light.SMTLib as SMTLib
import qualified Copilot.Kind.Light.TPTP as TPTP

import Control.Applicative
import Control.Monad (msum, unless, MonadPlus(..))
import Control.Monad.State (StateT, runStateT, lift, get, modify)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import qualified Copilot.Kind.Prover as P

import Data.Default
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (nub)

--------------------------------------------------------------------------------

data Options = Options
  { -- The maximum number of steps of the k-induction algorithm the prover runs
    -- before giving up.
    maxK :: Integer

    -- If `onlyBmc` is set to `True`, the prover will only search for
    -- counterexamples and won't try to prove the properties discharged to it.
  , onlyBmc  :: Bool

    -- If `debug` is set to `True`, the SMTLib queries produced by the prover
    -- are displayed in the standard output.
  , debug    :: Bool
  }

instance Default Options where
  def = Options
    { maxK  = 10
    , onlyBmc   = False
    , debug     = False
    }

kInduction :: SmtFormat a => Options -> Backend a -> P.Prover
kInduction options backend = P.Prover
  { P.proverName  = "Light"
  , P.hasFeature  = \case
      P.GiveCex -> False
      P.HandleAssumptions -> True
  , P.startProver = return . ProofState options backend Map.empty . translate
  , P.askProver   = kInduction' (maxK options)
  , P.closeProver = const $ return ()
  }

yices :: Backend SmtLib
yices = Backend
  { cmd             = "yices-smt2"
  , cmdOpts         = ["--incremental"]
  , inputTerminator = const $ return ()
  , incremental     = True
  , logic           = "QF_UFLIA"
  , interpret       = SMTLib.interpret
  }

altErgo :: Backend SmtLib
altErgo = Backend
  { cmd             = "alt-ergo.opt"
  , cmdOpts         = []
  , inputTerminator = hClose
  , incremental     = False
  , logic           = "QF_NRA"
  , interpret       = SMTLib.interpret
  }

dReal :: Backend SmtLib
dReal = Backend
  { cmd             = "perl"
  , cmdOpts         = ["-e", "alarm 5; exec dReal"]
  , inputTerminator = hClose
  , incremental     = False
  , logic           = "QF_NRA"
  , interpret       = SMTLib.interpret
  }

-- The argument is the path to the "tptp" subdirectory of the metitarski install location.
metit :: String -> Backend Tptp
metit installDir = Backend
  { cmd             = "metit"
  , cmdOpts         =
      [ "--time", "5"
      , "--autoInclude"
      , "--tptp", installDir
      , "/dev/stdin"
      ]
  , inputTerminator = hClose
  , incremental     = False
  , logic           = ""
  , interpret       = TPTP.interpret
  }

-------------------------------------------------------------------------------

-- | Checks the Copilot specification with k-induction

type ProofScript b = MaybeT (StateT (ProofState b) IO)

runPS :: ProofScript b a -> ProofState b -> IO (Maybe a, ProofState b)
runPS ps = runStateT (runMaybeT ps)

data ProofState b = ProofState
  { options :: Options
  , backend :: Backend b
  , solvers :: Map.Map SolverId (SMT.Solver b)
  , spec    :: IL
  }

data SolverId = Base | Step
  deriving (Show, Ord, Eq)

getOptions :: ProofScript b Options
getOptions = options <$> get

getBackend :: ProofScript b (Backend b)
getBackend = backend <$> get

getSpec :: ProofScript b IL
getSpec = spec <$> get

getModels :: [PropId] -> [PropId] -> ProofScript b ([Constraint], [Constraint], [Constraint])
getModels assumptionIds toCheckIds = do
  IL {modelInit, modelRec, properties} <- getSpec
  let assumptions = selectProps assumptionIds properties
      modelRec'   = modelRec ++ assumptions
      toCheck     = selectProps toCheckIds properties
  return (modelInit, modelRec', toCheck)

getSolvers :: ProofScript b (Map.Map SolverId (SMT.Solver b))
getSolvers = solvers <$> get

getSolver :: SmtFormat b => SolverId -> ProofScript b (SMT.Solver b)
getSolver sid = do
  solvers <- getSolvers
  case Map.lookup sid solvers of
    Nothing -> startNewSolver sid
    Just solver -> return solver

setSolver :: SolverId -> SMT.Solver b -> ProofScript b ()
setSolver sid solver =
  (lift . modify) $ \s -> s { solvers = Map.insert sid solver (solvers s) }

deleteSolver :: SolverId -> ProofScript b ()
deleteSolver sid =
  (lift . modify) $ \s -> s { solvers = Map.delete sid (solvers s) }

startNewSolver :: SmtFormat b => SolverId -> ProofScript b (SMT.Solver b)
startNewSolver sid = do
  opts <- getOptions
  backend <- getBackend
  solver <- liftIO $ SMT.startNewSolver (show sid) (debug opts) backend
  setSolver sid solver
  return solver

declVars :: SmtFormat b => SolverId -> [VarDescr] -> ProofScript b ()
declVars sid vs = do
  solver <- getSolver sid
  liftIO $ SMT.declVars solver vs
  setSolver sid $ SMT.updateVars solver vs

assume :: SmtFormat b => SolverId -> [Constraint] -> ProofScript b ()
assume sid cs = getSolver sid >>= \s -> liftIO $ SMT.assume s cs

entailed :: SmtFormat b => SolverId -> [Constraint] -> ProofScript b SatResult
entailed sid cs = do
  backend <- getBackend
  solver <- getSolver sid
  result <- liftIO $ SMT.entailed solver cs
  unless (incremental backend) $ stop sid
  return result

stop :: SmtFormat b => SolverId -> ProofScript b ()
stop id = do
  s <- getSolver id
  deleteSolver id
  liftIO $ SMT.stop s

halt :: ProofScript b a
halt = mzero

stopSolvers :: SmtFormat b => ProofScript b ()
stopSolvers = do
  solvers <- getSolvers
  mapM_ stop (fst <$> Map.toList solvers)

entailment :: SmtFormat b => SolverId -> [Constraint] -> [Constraint] -> ProofScript b SatResult
entailment sid assumptions props = do
  declVars sid $ nub $ getVars assumptions ++ getVars props
  assume sid assumptions
  entailed sid props

kInduction' :: SmtFormat b => Integer -> ProofState b -> [PropId] -> [PropId] -> IO P.Output
kInduction' k s as ps =
  (fromMaybe (P.Output P.Unknown [(if k /= 1 then "k-" else "") ++ "induction failed"]) . fst)
  <$> runPS (msum (map (induction as ps) $ range k) <* stopSolvers) s
  where range k = if k > 0 then [1 .. k] else [1 ..]

induction :: SmtFormat b => [PropId] -> [PropId] -> Integer -> ProofScript b P.Output
induction assumptionIds toCheckIds k = do

  (modelInit, modelRec, toCheck) <- getModels assumptionIds toCheckIds

  let base    = [evalAt (Fixed i) m | m <- modelRec, i <- [0 .. k]]
      baseInv = [evalAt (Fixed k) m | m <- toCheck]
  entailment Base (modelInit ++ base) baseInv >>= \case
    Sat     -> halt -- $ P.Output Invalid ["base case failed"]
    Unknown -> halt -- $ P.Output Unknown ["the base case solver was indecisive"]
    _       -> return ()

  let step    = [evalAt (_n_plus i) m | m <- modelRec, i <- [0 .. k]]
                ++ [evalAt (_n_plus i) m | m <- toCheck, i <- [0 .. k - 1]]
      stepInv = [evalAt (_n_plus k) m | m <- toCheck]
  entailment Step (modelInit ++ step) stepInv >>= \case
    Sat     -> halt -- $ P.Output Invalid ["inductive step failed"]
    Unknown -> halt -- $ P.Output Unknown ["the inductive case solver was indecisive"]
    Unsat   -> return $ P.Output P.Valid ["proved with k-induction, k = " ++ show k]

selectProps :: [PropId] -> Map.Map PropId Constraint -> [Constraint]
selectProps propIds properties =
  [c | (id, c) <- Map.toList properties, id `elem` propIds]

--------------------------------------------------------------------------------
