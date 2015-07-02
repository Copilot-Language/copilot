--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase, NamedFieldPuns, DeriveFunctor, FlexibleInstances #-}

module Copilot.Kind.Light.Prover
  ( module Data.Default
  , Options (..)
  , lightProver
  , yices, dReal, altErgo
  ) where

import Copilot.Kind.IL.Translate
import Copilot.Kind.IL

import System.IO (hClose)

import qualified Copilot.Core as Core
import qualified Copilot.Kind.Light.SMT as SMT

import Control.Applicative
import Control.Monad (unless, liftM)
import Control.Monad.State (StateT, runStateT, lift, get, modify)
import Control.Monad.Free

import Copilot.Kind.Prover

import Data.Default
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (intersectBy, nub)
import Data.Function (on)
import Data.Set (union, (\\), fromList, elems)

--------------------------------------------------------------------------------

data Options = Options
  { -- The maximum number of steps of the k-induction algorithm the prover runs
    -- before giving up.
    kTimeout :: Integer

    -- If `onlyBmc` is set to `True`, the prover will only search for
    -- counterexamples and won't try to prove the properties discharged to it.
  , onlyBmc  :: Bool

    -- If `debug` is set to `True`, the SMTLib queries produced by the prover
    -- are displayed in the standard output.
  , debug    :: Bool

  , backend  :: SMT.Backend
  }

instance Default Options where
  def = Options
    { kTimeout  = 100
    , onlyBmc   = False
    , debug     = False
    , backend   = yices
    }

lightProver :: Options -> Prover
lightProver options = Prover
  { proverName  = "Light"
  , hasFeature  = \case
      GiveCex -> False
      HandleAssumptions -> True
  , startProver = return . ProofState options Map.empty . translate
  , askProver   = kInduction 10
  , closeProver = const $ return ()
  }

yices = SMT.Backend
  { SMT.cmd             = "yices-smt2"
  , SMT.cmdOpts         = ["--incremental"]
  , SMT.logic           = "QF_UFLIA"
  , SMT.inputTerminator = const $ return ()
  , SMT.incremental     = True
  , SMT.format          = SMT.smtLib2
  }

altErgo = SMT.Backend
  { SMT.cmd             = "alt-ergo.opt"
  , SMT.cmdOpts         = []
  , SMT.logic           = "QF_UFLIA"
  , SMT.inputTerminator = hClose
  , SMT.incremental     = False
  , SMT.format          = SMT.smtLib2
  }

dReal = SMT.Backend
  { SMT.cmd             = "perl"
  -- Run dReal with a timeout.
  , SMT.cmdOpts         = ["-e", "alarm 10; exec dReal"]
  , SMT.logic           = "QF_NRA"
  , SMT.inputTerminator = hClose
  , SMT.incremental     = False
  , SMT.format          = SMT.smtLib2
  }

metit = SMT.Backend
  { SMT.cmd             = "metit"
  , SMT.cmdOpts         = ["--autoInclude"]
  , SMT.logic           = ""
  , SMT.inputTerminator = const $ return ()
  , SMT.incremental     = False
  , SMT.format          = SMT.tptp
  }

-------------------------------------------------------------------------------

-- | Checks the Copilot specification with k-induction

data ProofTerm a = StartNewSolver String Bool SMT.Backend (SMT.Solver -> a)
                 | DeclVars SMT.Solver [VarDescr] a
                 | Assume SMT.Solver [Constraint] a
                 | Entailed SMT.Solver [Constraint] (SMT.SatResult -> a)
                 | Stop SMT.Solver a
                 | Exit (Output, ProofState)
                 deriving (Functor)

type ProofScript = StateT ProofState StatelessPS
type StatelessPS = Free ProofTerm
data ProofState = ProofState
  { opts    :: Options
  , solvers :: Map.Map SolverId SMT.Solver
  , spec    :: IL
  }

data SolverId = Base | Step
  deriving (Show, Ord, Eq)

startNewSolver :: SolverId -> ProofScript SMT.Solver
startNewSolver sid = do
  opts <- opts <$> get
  solver <- liftF $ StartNewSolver (show sid) (debug opts) (backend opts) id
  modify $ \s@ProofState { solvers }
    -> s { solvers = Map.insert sid solver solvers }
  return solver

declVars :: SolverId -> [VarDescr] -> ProofScript ()
declVars sid vs = do
  solver <- getSolver sid
  liftF $ DeclVars solver vs ()
  modify $ \s@ProofState { solvers }
    -> s { solvers = Map.insert sid (SMT.updateVars solver vs) solvers }

assume :: SolverId -> [Constraint] -> ProofScript ()
assume sid cs = getSolver sid >>= \s -> liftF $ Assume s cs ()

entailed :: SolverId -> [Constraint] -> ProofScript SMT.SatResult
entailed sid cs = do
  opts <- opts <$> get
  solver <- getSolver sid
  result <- liftF $ Entailed solver cs id
  unless (SMT.incremental $ backend opts) $ stop sid
  return result

stop :: SolverId -> ProofScript ()
stop id = do
  s <- getSolver id
  modify $ \s@ProofState { solvers } -> s { solvers = Map.delete id solvers }
  liftF $ Stop s ()

halt :: Output -> ProofScript a
halt r = get >>= (liftF . Exit . ((,) r))

stopSolvers :: ProofScript ()
stopSolvers = do
  solvers <- solvers <$> get
  mapM_ (\s -> lift $ liftF $ Stop s ()) (snd <$> Map.toList solvers)

entailment :: SolverId -> [Constraint] -> [Constraint] -> ProofScript SMT.SatResult
entailment sid assumptions props = do
  declVars sid $ nub $ getVars assumptions ++ getVars props
  assume sid assumptions
  entailed sid props

getSolver :: SolverId -> ProofScript SMT.Solver
getSolver sid = do
  solvers <- solvers <$> get
  case Map.lookup sid solvers of
    Nothing -> startNewSolver sid
    Just solver -> return solver

getModels :: [PropId] -> [PropId] -> ProofScript ([Constraint], [Constraint], [Constraint])
getModels assumptionIds toCheckIds = do
  IL {modelInit, modelRec, properties} <- spec <$> get
  let assumptions = selectProps assumptionIds properties
      modelRec'   = modelRec ++ assumptions
      toCheck     = selectProps toCheckIds properties
  return (modelInit, modelRec', toCheck)

runSMTLib :: StatelessPS (Output, ProofState) -> IO (Output, ProofState)
runSMTLib (Pure v) = return v
runSMTLib (Free v) = case v of
  DeclVars s vs a             -> SMT.declVars s vs >> runSMTLib a
  Assume s cs a               -> SMT.assume s cs >> runSMTLib a
  StartNewSolver name dbg b f -> SMT.startNewSolver name dbg b >>= runSMTLib . f
  Entailed s cs f             -> SMT.entailed s cs >>= runSMTLib . f
  Stop s a                    -> SMT.stop s >> runSMTLib a
  Exit v                      -> return v

kInduction :: Integer -> ProofState -> [PropId] -> [PropId] -> IO Output
kInduction k st as ps = (foldl mplus mzero $ map (induction as ps) $ range k) >>= term
  where
    range k = if k > 0 then [1 .. k] else [1 ..]
    mzero = return (Output Unknown [""], st)
    mplus :: IO (Output, ProofState) -> ProofScript Output -> IO (Output, ProofState)
    mplus m b = m >>= \case
      r@(Output Valid _, _) -> return r
      (_, st)               -> runSMTLib $ runStateT b st
    term (o, st) = liftM fst $ runSMTLib $ runStateT (stopSolvers >> return o) st

induction :: [PropId] -> [PropId] -> Integer -> ProofScript Output
induction assumptionIds toCheckIds k = do

  (modelInit, modelRec, toCheck) <- getModels assumptionIds toCheckIds

  let base    = [evalAt (Fixed i) m | m <- modelRec, i <- [0 .. k]]
      baseInv = [evalAt (Fixed k) m | m <- toCheck]
  entailment Base (modelInit ++ base) baseInv >>= \case
    SMT.Sat     -> halt $ Output Invalid ["base case failed"]
    SMT.Unknown -> halt $ Output Unknown ["the base case solver was indecisive"]
    _           -> return ()

  let step    = [evalAt (_n_plus i) m | m <- modelRec, i <- [0 .. k]]
                ++ [evalAt (_n_plus i) m | m <- toCheck, i <- [0 .. k - 1]]
      stepInv = [evalAt (_n_plus k) m | m <- toCheck]
  entailment Step (modelInit ++ step) stepInv >>= \case
    SMT.Sat     -> halt $ Output Invalid ["inductive step failed"]
    SMT.Unknown -> halt $ Output Unknown ["the inductive case solver was indecisive"]
    SMT.Unsat   -> return $ Output Valid ["proved with k-induction, k = " ++ show k]

selectProps :: [PropId] -> Map.Map PropId Constraint -> [Constraint]
selectProps propIds properties =
  [c | (id, c) <- Map.toList properties, id `elem` propIds]

--------------------------------------------------------------------------------
