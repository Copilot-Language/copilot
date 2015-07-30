--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase, NamedFieldPuns, FlexibleInstances, RankNTypes, GADTs #-}

module Copilot.Kind.Light.Prover
  ( module Data.Default
  , Options (..)
  , Strategy (..)
  , kInduction
  , yices, dReal, altErgo, metit, z3, cvc4, mathsat
  , Backend, SmtFormat
  , SmtLib, Tptp
  ) where

import Copilot.Kind.IL.Translate
import Copilot.Kind.IL
import Copilot.Kind.Prover (Output (..))
import qualified Copilot.Kind.Prover as P

import Copilot.Kind.Light.Backend
import qualified Copilot.Kind.Light.SMT as SMT

import Copilot.Kind.Light.SMTLib (SmtLib)
import Copilot.Kind.Light.TPTP (Tptp)
import qualified Copilot.Kind.Light.SMTLib as SMTLib
import qualified Copilot.Kind.Light.TPTP as TPTP

import Control.Applicative ((<$>), (<*))
import Control.Monad (msum, unless, mzero, when)
import Control.Monad.State (StateT, runStateT, lift, get, modify)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))

import Data.Word
import Data.Maybe (fromJust, fromMaybe)
import Data.Function (on)
import Data.Default (Default(..))
import qualified Data.Map as Map
import Copilot.Kind.Misc.Utils

import System.IO (hClose)

--------------------------------------------------------------------------------

data Options = Options
  -- The maximum number of steps of the k-induction algorithm the prover runs
  -- before giving up.
  { strategy :: Strategy

  -- If `debug` is set to `True`, the SMTLib/TPTP queries produced by the
  -- prover are displayed in the standard output.
  , debug   :: Bool
  }

data Strategy = OnlySat | Unbounded | Bounded Word32

instance Default Options where
  def = Options
    { strategy = Bounded 10
    , debug    = False
    }

kInduction :: SmtFormat a => Options -> Backend a -> P.Prover
kInduction options backend = P.Prover
  { P.proverName  = "k-induction"
  , P.hasFeature  = \case
      P.GiveCex -> False
      P.HandleAssumptions -> True
  , P.startProver = return . ProofState options backend Map.empty . translate
  , P.askProver   = kInduction' (strategy options)
  , P.closeProver = const $ return ()
  }

yices :: Backend SmtLib
yices = Backend
  { name            = "Yices"
  , cmd             = "yices-smt2"
  , cmdOpts         = ["--incremental"]
  , inputTerminator = const $ return ()
  , incremental     = True
  , logic           = "QF_UFLIRA"
  , interpret       = SMTLib.interpret
  }

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

-- The argument is the path to the "tptp" subdirectory of the metitarski
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
  , solvers :: Map.Map SolverId (SMT.Solver b)
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
  opts <- options <$> get
  backend <- backend <$> get
  solver <- liftIO $ SMT.startNewSolver (show sid) (debug opts) backend
  setSolver sid solver
  return solver

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
  solver <- getSolver sid
  result <- liftIO $ SMT.entailed solver cs
  unless (incremental backend) $ stop sid
  return result

stop :: SmtFormat b => SolverId -> ProofScript b ()
stop id = do
  s <- getSolver id
  deleteSolver id
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
          ConstI _             -> []
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

kInduction' :: SmtFormat b => Strategy -> ProofState b -> [PropId] -> [PropId] -> IO Output
kInduction' strat s as ps = case strat of
  OnlySat -> (fromJust . fst) <$> runPS ((onlySat as ps) <* stopSolvers) s
  Bounded k -> kinduct [0..toInteger k]
  Unbounded -> kinduct [0..]
  where kinduct ks = (fromMaybe (Output P.Unknown ["proof by k-induction failed"]) . fst)
                     <$> runPS (msum (map (induction as ps) ks) <* stopSolvers) s

onlySat :: SmtFormat b => [PropId] -> [PropId] -> ProofScript b Output
onlySat assumptionIds toCheckIds = do

  (modelInit, modelRec, toCheck, inductive) <- getModels assumptionIds toCheckIds

  let base    = map (evalAt (Fixed 0)) modelRec
      baseInv = map (evalAt (Fixed 0)) toCheck

  when inductive $ liftIO $
    putStrLn "Warning: OnlySat doesn't really work for inductive props."

  entailment Base (modelInit ++ base) (map (Op1 Bool Not) baseInv) >>= \case
    Unsat   -> invalid "prop not satisfiable"
    Unknown -> unknown' "failed to find a satisfying model"
    Sat     -> sat "prop is satisfiable"


induction :: SmtFormat b => [PropId] -> [PropId] -> Integer -> ProofScript b Output
induction assumptionIds toCheckIds k = do

  (modelInit, modelRec, toCheck, inductive) <- getModels assumptionIds toCheckIds

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

selectProps :: [PropId] -> Map.Map PropId ([Expr], Expr) -> ([Expr], [Expr])
selectProps propIds properties =
  (squash . unzip) [(as, p) | (id, (as, p)) <- Map.toList properties, id `elem` propIds]
    where squash (a, b) = (concat a, b)

--------------------------------------------------------------------------------
