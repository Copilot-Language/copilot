--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}

-- | Connections to various SMT solvers and theorem provers.
module Copilot.Theorem.Prover.SMT
  (

    -- * Backends
    Backend
  , SmtFormat
  , SmtLib
  , Tptp
  , yices, dReal, altErgo, metit, z3, cvc4, mathsat

    -- * Tactics
  , Options (..)
  , induction, kInduction, onlySat, onlyValidity

    -- * Auxiliary
  , module Data.Default
  ) where

import Copilot.Theorem.IL.Translate
import Copilot.Theorem.IL
import Copilot.Theorem.Prove (Output (..), check, Proof, Universal, Existential)
import qualified Copilot.Theorem.Prove as P

import Copilot.Theorem.Prover.Backend
import qualified Copilot.Theorem.Prover.SMTIO as SMT

import Copilot.Theorem.Prover.SMTLib (SmtLib)
import Copilot.Theorem.Prover.TPTP (Tptp)
import qualified Copilot.Theorem.Prover.SMTLib as SMTLib
import qualified Copilot.Theorem.Prover.TPTP as TPTP

import Control.Monad (msum, unless, mzero)
import Control.Monad.State (StateT, runStateT, lift, get, modify)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))

import Data.Word
import Data.Maybe (fromJust, fromMaybe)
import Data.Function (on)
import Data.Default (Default(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Copilot.Theorem.Misc.Utils

import System.IO (hClose)

--------------------------------------------------------------------------------

-- * Tactics

-- | Options to configure the provers.
data Options = Options
  { startK :: Word32
    -- ^ Initial @k@ for the k-induction algorithm.

  , maxK :: Word32
    -- ^ The maximum number of steps of the k-induction algorithm the prover runs
    -- before giving up.

  , debug :: Bool
    -- ^ If @debug@ is set to @True@, the SMTLib/TPTP queries produced by the
    -- prover are displayed in the standard output.
  }

-- | Default 'Options' with a @0@ @k@ and a max of @10@ steps, and that produce
-- no debugging info.
instance Default Options where
  def = Options
    { startK = 0
    , maxK   = 10
    , debug  = False
    }

-- | Tactic to find only a proof of satisfiability.
onlySat :: SmtFormat a => Options -> Backend a -> Proof Existential
onlySat opts backend = check P.Prover
  { P.proverName  = "OnlySat"
  , P.startProver = return . ProofState opts backend Map.empty . translateWithBounds
  , P.askProver   = onlySat'
  , P.closeProver = const $ return ()
  }

-- | Tactic to find only a proof of validity.
onlyValidity :: SmtFormat a => Options -> Backend a -> Proof Universal
onlyValidity opts backend = check P.Prover
  { P.proverName  = "OnlyValidity"
  , P.startProver = return . ProofState opts backend Map.empty . translateWithBounds
  , P.askProver   = onlyValidity'
  , P.closeProver = const $ return ()
  }

-- | Tactic to find a proof by standard 1-induction.
--
-- The values for @startK@ and @maxK@ in the options are ignored.
induction :: SmtFormat a => Options -> Backend a -> Proof Universal
induction opts backend = check P.Prover
  { P.proverName  = "Induction"
  , P.startProver = return . ProofState opts backend Map.empty . translateWithBounds
  , P.askProver   = kInduction' 0 0
  , P.closeProver = const $ return ()
  }

-- | Tactic to find a proof by k-induction.
kInduction :: SmtFormat a => Options -> Backend a -> Proof Universal
kInduction opts backend = check P.Prover
  { P.proverName  = "K-Induction"
  , P.startProver = return . ProofState opts backend Map.empty . translateWithBounds
  , P.askProver   = kInduction' (startK opts) (maxK opts)
  , P.closeProver = const $ return ()
  }

-------------------------------------------------------------------------------

-- * Backends

-- | Backend to the Yices 2 SMT solver.
--
-- It enables non-linear arithmetic (@QF_NRA@), which means MCSat will be used.
--
-- The command @yices-smt2@ must be in the user's @PATH@.
yices :: Backend SmtLib
yices = Backend
  { name            = "Yices"
  , cmd             = "yices-smt2"
  , cmdOpts         = ["--incremental"]
  , inputTerminator = const $ return ()
  , incremental     = True
  , logic           = "QF_NRA"
  , interpret       = SMTLib.interpret
  }

-- | Backend to the cvc4 SMT solver.
--
-- It enables support for uninterpreted functions and mixed nonlinear
-- arithmetic (@QF_NIRA@).
--
-- The command @cvc4@ must be in the user's @PATH@.
cvc4 :: Backend SmtLib
cvc4 = Backend
  { name            = "CVC4"
  , cmd             = "cvc4"
  , cmdOpts         = ["--incremental", "--lang=smt2", "--tlimit-per=5000"]
  , inputTerminator = const $ return ()
  , incremental     = True
  , logic           = "QF_UFNIRA"
  , interpret       = SMTLib.interpret
  }

-- | Backend to the Alt-Ergo SMT solver.
--
-- It enables support for uninterpreted functions and mixed nonlinear
-- arithmetic (@QF_NIRA@).
--
-- The command @alt-ergo.opt@ must be in the user's @PATH@.
altErgo :: Backend SmtLib
altErgo = Backend
  { name            = "Alt-Ergo"
  , cmd             = "alt-ergo.opt"
  , cmdOpts         = []
  , inputTerminator = hClose
  , incremental     = False
  , logic           = "QF_UFNIRA"
  , interpret       = SMTLib.interpret
  }

-- | Backend to the Z3 theorem prover.
--
-- The command @z3@ must be in the user's @PATH@.
z3 :: Backend SmtLib
z3 = Backend
  { name            = "Z3"
  , cmd             = "z3"
  , cmdOpts         = ["-smt2", "-in"]
  , inputTerminator = const $ return ()
  , incremental     = True
  , logic           = ""
  , interpret       = SMTLib.interpret
  }

-- | Backend to the dReal SMT solver.
--
-- It enables non-linear arithmetic (@QF_NRA@).
--
-- The libraries for dReal must be installed and @perl@ must be in the user's
-- @PATH@.
dReal :: Backend SmtLib
dReal = Backend
  { name            = "dReal"
  , cmd             = "perl"
  , cmdOpts         = ["-e", "alarm 10; exec dReal"]
  , inputTerminator = hClose
  , incremental     = False
  , logic           = "QF_NRA"
  , interpret       = SMTLib.interpret
  }

-- | Backend to the Mathsat SMT solver.
--
-- It enables non-linear arithmetic (@QF_NRA@).
--
-- The command @mathsat@ must be in the user's @PATH@.
mathsat :: Backend SmtLib
mathsat = Backend
  { name            = "MathSAT"
  , cmd             = "mathsat"
  , cmdOpts         = []
  , inputTerminator = const $ return ()
  , incremental     = True
  , logic           = "QF_NRA"
  , interpret       = SMTLib.interpret
  }

-- | Backend to the MetiTaski theorem prover.
--
-- The command @metit@ must be in the user's @PATH@.
--
-- The argument string is the path to the @tptp@ subdirectory of the metitarski
-- install location.
metit :: String -> Backend Tptp
metit installDir = Backend
  { name            = "MetiTarski"
  , cmd             = "metit"
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
  , solvers :: Map SolverId (SMT.Solver b)
  , spec    :: IL
  }

data SolverId = Base | Step
  deriving (Show, Ord, Eq)

getModels :: [PropId] -> [PropId] -> ProofScript b ([Expr], [Expr], [Expr], Bool)
getModels assumptionIds toCheckIds = do
  IL {modelInit, modelRec, properties, inductive} <- spec <$> get
  let (as, as')       = selectProps assumptionIds properties
      (as'', toCheck) = selectProps toCheckIds properties
      modelRec'       = modelRec ++ as ++ as' ++ as''
  return (modelInit, modelRec', toCheck, inductive)

getSolver :: SmtFormat b => SolverId -> ProofScript b (SMT.Solver b)
getSolver sid = do
  solvers <- solvers <$> get
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
  dbg <- (options <$> get >>= return . debug)
  backend <- backend <$> get
  s <- liftIO $ SMT.startNewSolver (show sid) dbg backend
  setSolver sid s
  return s

declVars :: SmtFormat b => SolverId -> [VarDescr] -> ProofScript b ()
declVars sid vs = do
  s <- getSolver sid
  s' <- liftIO $ SMT.declVars s vs
  setSolver sid s'

assume :: SmtFormat b => SolverId -> [Expr] -> ProofScript b ()
assume sid cs = do
  s <- getSolver sid
  s' <- liftIO $ SMT.assume s cs
  setSolver sid s'

entailed :: SmtFormat b => SolverId -> [Expr] -> ProofScript b SatResult
entailed sid cs = do
  backend <- backend <$> get
  s <- getSolver sid
  result <- liftIO $ SMT.entailed s cs
  unless (incremental backend) $ stop sid
  return result

stop :: SmtFormat b => SolverId -> ProofScript b ()
stop sid = do
  s <- getSolver sid
  deleteSolver sid
  liftIO $ SMT.stop s

proofKind :: Integer -> String
proofKind 0 = "induction"
proofKind k = "k-induction (k = " ++ show k ++ ")"

stopSolvers :: SmtFormat b => ProofScript b ()
stopSolvers = do
  solvers <- solvers <$> get
  mapM_ stop (fst <$> Map.toList solvers)

entailment :: SmtFormat b => SolverId -> [Expr] -> [Expr] -> ProofScript b SatResult
entailment sid assumptions props = do
  declVars sid $ nub' $ getVars assumptions ++ getVars props
  assume sid assumptions
  entailed sid props

getVars :: [Expr] -> [VarDescr]
getVars = nubBy' (compare `on` varName) . concatMap getVars'
  where getVars' :: Expr -> [VarDescr]
        getVars' = \case
          ConstB _             -> []
          ConstI _ _           -> []
          ConstR _             -> []
          Ite _ e1 e2 e3       -> getVars' e1 ++ getVars' e2 ++ getVars' e3
          Op1 _ _ e            -> getVars' e
          Op2 _ _ e1 e2        -> getVars' e1 ++ getVars' e2
          SVal t seq (Fixed i) -> [VarDescr (seq ++ "_" ++ show i) t []]
          SVal t seq (Var i)   -> [VarDescr (seq ++ "_n" ++ show i) t []]
          FunApp t name args   -> [VarDescr name t (map typeOf args)]
                                  ++ concatMap getVars' args

unknown :: ProofScript b a
unknown = mzero

unknown' :: String -> ProofScript b Output
unknown' msg = return $ Output P.Unknown [msg]

invalid :: String -> ProofScript b Output
invalid msg = return $ Output P.Invalid [msg]

sat :: String -> ProofScript b Output
sat msg = return $ Output P.Sat [msg]

valid :: String -> ProofScript b Output
valid msg = return $ Output P.Valid [msg]

kInduction' :: SmtFormat b => Word32 -> Word32 -> ProofState b -> [PropId] -> [PropId] -> IO Output
kInduction' startK maxK s as ps = (fromMaybe (Output P.Unknown ["proof by k-induction failed"]) . fst)
  <$> runPS (msum (map induction [(toInteger startK) .. (toInteger maxK)]) <* stopSolvers) s
  where
    induction k = do
      (modelInit, modelRec, toCheck, inductive) <- getModels as ps

      let base    = [evalAt (Fixed i) m | m <- modelRec, i <- [0 .. k]]
          baseInv = [evalAt (Fixed k) m | m <- toCheck]

      let step    = [evalAt (_n_plus i) m | m <- modelRec, i <- [0 .. k + 1]]
                    ++ [evalAt (_n_plus i) m | m <- toCheck, i <- [0 .. k]]
          stepInv = [evalAt (_n_plus $ k + 1) m | m <- toCheck]

      entailment Base (modelInit ++ base) baseInv >>= \case
        Sat     -> invalid $ "base case failed for " ++ proofKind k
        Unknown -> unknown
        Unsat   ->
          if not inductive then valid ("proved without induction")
          else entailment Step step stepInv >>= \case
            Sat     -> unknown
            Unknown -> unknown
            Unsat   -> valid $ "proved with " ++ proofKind k


onlySat' :: SmtFormat b => ProofState b -> [PropId] -> [PropId] -> IO Output
onlySat' s as ps = (fromJust . fst) <$> runPS (script <* stopSolvers) s
  where
    script  = do
      (modelInit, modelRec, toCheck, inductive) <- getModels as ps

      let base    = map (evalAt (Fixed 0)) modelRec
          baseInv = map (evalAt (Fixed 0)) toCheck

      if inductive
        then unknown' "proposition requires induction to prove."
        else entailment Base (modelInit ++ base) (map (Op1 Bool Not) baseInv) >>= \case
          Unsat   -> invalid "prop not satisfiable"
          Unknown -> unknown' "failed to find a satisfying model"
          Sat     -> sat "prop is satisfiable"

onlyValidity' :: SmtFormat b => ProofState b -> [PropId] -> [PropId] -> IO Output
onlyValidity' s as ps = (fromJust . fst) <$> runPS (script <* stopSolvers) s
  where
    script  = do
      (modelInit, modelRec, toCheck, inductive) <- getModels as ps

      let base    = map (evalAt (Fixed 0)) modelRec
          baseInv = map (evalAt (Fixed 0)) toCheck

      if inductive
        then unknown' "proposition requires induction to prove."
        else entailment Base (modelInit ++ base) baseInv >>= \case
          Unsat   -> valid "proof by SMT solver"
          Unknown -> unknown
          Sat     -> invalid "SMT solver found a counter-example."

selectProps :: [PropId] -> Map PropId ([Expr], Expr) -> ([Expr], [Expr])
selectProps propIds properties =
  (squash . unzip) [(as, p) | (id, (as, p)) <- Map.toList properties, id `elem` propIds]
    where squash (a, b) = (concat a, b)

--------------------------------------------------------------------------------
