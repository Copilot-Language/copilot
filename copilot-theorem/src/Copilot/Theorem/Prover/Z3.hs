--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE Trustworthy           #-}

module Copilot.Theorem.Prover.Z3
  ( module Data.Default
  , Options (..)
  , induction, kInduction, onlySat, onlyValidity
  )
where

import Copilot.Theorem.IL.Translate
import Copilot.Theorem.IL
import Copilot.Theorem.Prove (Output (..), check, Proof, Universal, Existential)
import qualified Copilot.Theorem.Prove as P

import Control.Monad (msum, mzero, when, void, unless)
import Control.Monad.State (StateT, runStateT, get, modify)
import Control.Monad.Trans.Maybe (MaybeT (..))

import Data.Word
import Data.Unit (Unit(..))
import Data.Maybe (fromJust, fromMaybe)
import Data.Default (Default(..))
import Data.List (foldl')

import Data.Set (Set, (\\), union)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Language.SMTLib2
import Language.SMTLib2.Pipe
import Language.SMTLib2.Connection
import Language.SMTLib2.Strategy

import Language.SMTLib2.Internals hiding (Var)

import System.Console.ANSI
import System.IO
import Control.Monad.Trans

--------------------------------------------------------------------------------

-- | Tactics

data Options = Options
  { nraNLSat :: Bool
  , startK   :: Word32
  -- The maximum number of steps of the k-induction algorithm the prover runs
  -- before giving up.
  , maxK     :: Word32

  -- If `debug` is set to `True`, the SMTLib/TPTP queries produced by the
  -- prover are displayed in the standard output.
  , debug    :: Bool
  }

instance Default Options where
  def = Options
    { nraNLSat = True
    , startK = 0
    , maxK   = 10
    , debug  = False
    }

onlySat :: Options -> Proof Existential
onlySat opts = check P.Prover
  { P.proverName  = "OnlySat"
  , P.startProver = return . ProofState opts Map.empty Map.empty Map.empty . translate
  , P.askProver   = onlySat'
  , P.closeProver = const $ return ()
  }

onlyValidity :: Options -> Proof Universal
onlyValidity opts = check P.Prover
  { P.proverName  = "OnlyValidity"
  , P.startProver = return . ProofState opts Map.empty Map.empty Map.empty . translate
  , P.askProver   = onlyValidity'
  , P.closeProver = const $ return ()
  }

induction :: Options -> Proof Universal
induction opts = check P.Prover
  { P.proverName  = "Induction"
  , P.startProver = return . ProofState opts Map.empty Map.empty Map.empty . translate
  , P.askProver   = kInduction' 0 0
  , P.closeProver = const $ return ()
  }

kInduction :: Options -> Proof Universal
kInduction opts = check P.Prover
  { P.proverName  = "K-Induction"
  , P.startProver = return . ProofState opts Map.empty Map.empty Map.empty . translate
  , P.askProver   = kInduction' (startK opts) (maxK opts)
  , P.closeProver = const $ return ()
  }

-------------------------------------------------------------------------------

-- | Checks the Copilot specification with k-induction

type Solver = SMTConnection (DebugBackend SMTPipe)

type ProofScript = MaybeT (StateT ProofState IO)

runPS :: ProofScript a -> ProofState -> IO (Maybe a, ProofState)
runPS ps = runStateT (runMaybeT ps)

data ProofState = ProofState
  { options  :: Options
  , solvers  :: Map SolverId Solver
  , vars     :: Map SolverId TransState
  , assumps  :: Map SolverId (Set Expr)
  , spec     :: IL
  }

data SolverId = Base | Step
  deriving (Show, Ord, Eq)

getModels :: [PropId] -> [PropId] -> ProofScript ([Expr], [Expr], [Expr], [Expr], Bool)
getModels assumptionIds toCheckIds = do
  IL {modelInit, modelRec, properties, inductive} <- spec <$> get
  let (as, as')       = selectProps assumptionIds properties
      (as'', toCheck) = selectProps toCheckIds properties
  return (as ++ as', modelInit, modelRec ++ as ++ as' ++ as'', toCheck, inductive)

getSolver :: SolverId -> ProofScript Solver
getSolver sid = do
  solvers <- solvers <$> get
  case Map.lookup sid solvers of
    Nothing -> startNewSolver sid
    Just solver -> return solver

setSolver :: SolverId -> Solver -> ProofScript ()
setSolver sid solver =
  (lift . modify) $ \s -> s { solvers = Map.insert sid solver (solvers s) }

getVars :: SolverId -> ProofScript TransState
getVars sid = do
  vars <- vars <$> get
  return $ case Map.lookup sid vars of
    Nothing -> noVars
    Just vs -> vs

setVars :: SolverId -> TransState -> ProofScript ()
setVars sid vs =
  (lift . modify) $ \s -> s { vars = Map.insert sid vs (vars s) }

newAssumps :: SolverId -> [Expr] -> ProofScript [Expr]
newAssumps sid as' = do
  assumps <- assumps <$> get
  case Map.lookup sid assumps of
    Nothing -> do
      modify $ \s -> s { assumps = Map.insert sid (Set.fromList as') assumps }
      return as'
    Just as -> do
      let as'' = (Set.fromList as') `union` as
      modify $ \s -> s { assumps = Map.insert sid as'' assumps }
      return $ Set.elems $ (Set.fromList as') \\ as

deleteSolver :: SolverId -> ProofScript ()
deleteSolver sid =
  (lift . modify) $ \s -> s { solvers = Map.delete sid (solvers s) }

startNewSolver :: SolverId -> ProofScript Solver
startNewSolver sid = do
  pipe <- liftIO $ createSMTPipe "z3" ["-smt2", "-in"]
  dbg <- (options <$> get >>= return . debug)
  s <- liftIO $ open (namedDebugBackend (show sid) (not dbg) pipe)
  setSolver sid s
  return s

stop :: SolverId -> ProofScript ()
stop sid = do
  s <- getSolver sid
  deleteSolver sid
  liftIO $ close s

stopSolvers :: ProofScript ()
stopSolvers = do
  solvers <- solvers <$> get
  mapM_ stop (fst <$> Map.toList solvers)

proofKind :: Integer -> String
proofKind 0 = "induction"
proofKind k = "k-induction (k = " ++ show k ++ ")"

entailment :: SolverId -> [Expr] -> [Expr] -> ProofScript CheckSatResult
entailment sid assumptions props = do
  s <- getSolver sid
  liftIO $ performSMT s $ setOption (ProduceModels True)
  -- liftIO $ performSMT s $ setOption (ProduceProofs True)
  -- liftIO $ performSMT s $ setOption (ProduceUnsatCores True)
  vs <- getVars sid
  assumps' <- newAssumps sid assumptions
  (_, vs')  <- liftIO $ performSMT s $ runStateT (mapM_ (\e -> transB e >>= lift . assert) assumps') vs
  setVars sid vs'
  liftIO $ performSMT s $ push
  _ <- liftIO $ performSMT s $ runStateT
    (transB (bsimpl (foldl' (Op2 Bool Or) (ConstB False) $ map (Op1 Bool Not) props)) >>= lift . assert) vs'

  nraNL <- (options <$> get >>= return . nraNLSat)
  res <- if nraNL
    then liftIO $ performSMT s $ checkSat' (Just (UsingParams (CustomTactic "qfnra-nlsat") []))
         (CheckSatLimits (Just 5000) Nothing)
    else liftIO $ performSMT s $ checkSat' Nothing (CheckSatLimits (Just 5000) Nothing)

  when (res == Sat) $ void $ liftIO $ performSMT s $ getModel
  -- when (res == Unsat) $ void $ liftIO $ performSMT s $ getProof
  liftIO $ performSMT s $ pop
  -- liftIO $ print model
  return res

unknown :: ProofScript a
unknown = mzero

unknown' :: String -> ProofScript Output
unknown' msg = return $ Output P.Unknown [msg]

invalid :: String -> ProofScript Output
invalid msg = return $ Output P.Invalid [msg]

sat :: String -> ProofScript Output
sat msg = return $ Output P.Sat [msg]

valid :: String -> ProofScript Output
valid msg = return $ Output P.Valid [msg]

kInduction' :: Word32 -> Word32 -> ProofState -> [PropId] -> [PropId] -> IO Output
kInduction' startK maxK s as ps = (fromMaybe (Output P.Unknown ["proof by " ++ proofKind (toInteger maxK) ++ " failed"]) . fst)
  <$> runPS (msum (map induction [(toInteger startK) .. (toInteger maxK)]) <* stopSolvers) s
  where
    induction k = do
      (assumps, modelInit, modelRec, toCheck, inductive) <- getModels as ps

      let base    = [evalAt (Fixed i) m | m <- modelRec, i <- [0 .. k]]
          baseInv = [evalAt (Fixed k) m | m <- toCheck]

      let step    = [evalAt (_n_plus i) m | m <- modelRec, i <- [0 .. k + 1]]
                    ++ [evalAt (_n_plus i) m | m <- toCheck, i <- [0 .. k]]
          stepInv = [evalAt (_n_plus $ k + 1) m | m <- toCheck]

      entailment Base assumps [ConstB False] >>= \case
        Unknown -> unknown
        Unsat   -> invalid $ "inconsistent assumptions"
        Sat     -> entailment Base (modelInit ++ base) baseInv >>= \case
          Sat     -> invalid $ "base case failed for " ++ proofKind k
          Unknown -> unknown
          Unsat   ->
            if not inductive then valid ("proved without induction")
            else entailment Step step stepInv >>= \case
              Sat     -> unknown
              Unknown -> unknown
              Unsat   -> valid $ "proved with " ++ proofKind k

onlySat' :: ProofState -> [PropId] -> [PropId] -> IO Output
onlySat' s as ps = (fromJust . fst) <$> runPS (script <* stopSolvers) s
  where
    script  = do
      (assumps, modelInit, modelRec, toCheck, inductive) <- getModels as ps

      let base    = map (evalAt (Fixed 0)) modelRec
          baseInv = map (evalAt (Fixed 0)) toCheck

      entailment Base assumps [ConstB False] >>= \case
        Unknown -> unknown
        Unsat   -> invalid $ "inconsistent assumptions"
        Sat     -> if inductive
          then unknown' "proposition requires induction to prove."
          else entailment Base (modelInit ++ base) (map (Op1 Bool Not) baseInv) >>= \case
            Unsat   -> invalid "prop not satisfiable"
            Unknown -> unknown' "failed to find a satisfying model"
            Sat     -> sat "prop is satisfiable"

onlyValidity' :: ProofState -> [PropId] -> [PropId] -> IO Output
onlyValidity' s as ps = (fromJust . fst) <$> runPS (script <* stopSolvers) s
  where
    script  = do
      (assumps, modelInit, modelRec, toCheck, inductive) <- getModels as ps

      let base    = map (evalAt (Fixed 0)) modelRec
          baseInv = map (evalAt (Fixed 0)) toCheck

      entailment Base assumps [ConstB False] >>= \case
        Unknown -> unknown
        Unsat   -> invalid $ "inconsistent assumptions"
        Sat     -> if inductive
          then unknown' "proposition requires induction to prove."
          else entailment Base (modelInit ++ base) baseInv >>= \case
            Unsat   -> valid "proof by Z3"
            Unknown -> unknown
            Sat     -> invalid "Z3 found a counter-example."

selectProps :: [PropId] -> Map PropId ([Expr], Expr) -> ([Expr], [Expr])
selectProps propIds properties =
  (squash . unzip) [(as, p) | (id, (as, p)) <- Map.toList properties, id `elem` propIds]
    where squash (a, b) = (concat a, b)

--------------------------------------------------------------------------------

-- | This is all very ugly. It might make better sense to go straight from Core to SMTExpr, or maybe use SBV instead.

type Trans = StateT TransState SMT

data TransState = TransState
  { boolVars :: Map String (SMTExpr Bool)
  , bv8Vars  :: Map String (SMTExpr BV8)
  , bv16Vars :: Map String (SMTExpr BV16)
  , bv32Vars :: Map String (SMTExpr BV32)
  , bv64Vars :: Map String (SMTExpr BV64)
  , ratVars  :: Map String (SMTExpr Rational)
  }

noVars :: TransState
noVars = TransState Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

getBoolVar :: String -> Trans (SMTExpr Bool)
getBoolVar = getVar boolVars (\v s -> s {boolVars  = v})

getBV8Var  :: String -> Trans (SMTExpr BV8)
getBV8Var  = getVar bv8Vars  (\v s -> s {bv8Vars  = v})

getBV16Var :: String -> Trans (SMTExpr BV16)
getBV16Var = getVar bv16Vars (\v s -> s {bv16Vars = v})

getBV32Var :: String -> Trans (SMTExpr BV32)
getBV32Var = getVar bv32Vars (\v s -> s {bv32Vars = v})

getBV64Var :: String -> Trans (SMTExpr BV64)
getBV64Var = getVar bv64Vars (\v s -> s {bv64Vars = v})

getRatVar  :: String -> Trans (SMTExpr Rational)
getRatVar  = getVar ratVars  (\v s -> s {ratVars  = v})

getVar :: (Unit (SMTAnnotation t), SMTType t) => (TransState -> Map String (SMTExpr t)) -> (Map String (SMTExpr t) -> TransState -> TransState) -> String -> Trans (SMTExpr t)
getVar proj upd v = do
  vs <- proj <$> get
  case Map.lookup v vs of
    Nothing -> do
      newVar <- lift $ varNamed v
      modify $ upd $ Map.insert v newVar vs
      return newVar
    Just x -> return x

transB :: Expr -> Trans (SMTExpr Bool)
transB = \case
  ConstB b           -> return $ constant b
  Ite _ c e1 e2      -> ite <$> transB c <*> transB e1 <*> transB e2
  Op1 _ Not e        -> not' <$> transB e
  Op2 _ And e1 e2    -> (.&&.) <$> transB e1 <*> transB e2
  Op2 _ Or e1 e2     -> (.||.) <$> transB e1 <*> transB e2
  Op2 _ Eq e1 e2     -> case typeOf e1 of
    Bool   -> (.==.) <$> transB e1    <*> transB e2
    Real   -> (.==.) <$> transR e1    <*> transR e2
    BV8    -> (.==.) <$> transBV8 e1  <*> transBV8 e2
    BV16   -> (.==.) <$> transBV16 e1 <*> transBV16 e2
    BV32   -> (.==.) <$> transBV32 e1 <*> transBV32 e2
    BV64   -> (.==.) <$> transBV64 e1 <*> transBV64 e2
    SBV8   -> (.==.) <$> transBV8 e1  <*> transBV8 e2
    SBV16  -> (.==.) <$> transBV16 e1 <*> transBV16 e2
    SBV32  -> (.==.) <$> transBV32 e1 <*> transBV32 e2
    SBV64  -> (.==.) <$> transBV64 e1 <*> transBV64 e2
  e@(Op2 _ Le e1 e2) -> case typeOf e1 of
    Bool   -> error $ "Comparing Bools: " ++ show e
    Real   -> (.<=.) <$> transR e1    <*> transR e2
    BV8    -> bvule  <$> transBV8 e1  <*> transBV8 e2
    BV16   -> bvule  <$> transBV16 e1 <*> transBV16 e2
    BV32   -> bvule  <$> transBV32 e1 <*> transBV32 e2
    BV64   -> bvule  <$> transBV64 e1 <*> transBV64 e2
    SBV8   -> bvule  <$> transBV8 e1  <*> transBV8 e2
    SBV16  -> bvule  <$> transBV16 e1 <*> transBV16 e2
    SBV32  -> bvule  <$> transBV32 e1 <*> transBV32 e2
    SBV64  -> bvule  <$> transBV64 e1 <*> transBV64 e2
  e@(Op2 _ Ge e1 e2) -> case typeOf e1 of
    Bool   -> error $ "Comparing Bools: " ++ show e
    Real   -> (.>=.) <$> transR e1    <*> transR e2
    BV8    -> bvuge  <$> transBV8 e1  <*> transBV8 e2
    BV16   -> bvuge  <$> transBV16 e1 <*> transBV16 e2
    BV32   -> bvuge  <$> transBV32 e1 <*> transBV32 e2
    BV64   -> bvuge  <$> transBV64 e1 <*> transBV64 e2
    SBV8   -> bvuge  <$> transBV8 e1  <*> transBV8 e2
    SBV16  -> bvuge  <$> transBV16 e1 <*> transBV16 e2
    SBV32  -> bvuge  <$> transBV32 e1 <*> transBV32 e2
    SBV64  -> bvuge  <$> transBV64 e1 <*> transBV64 e2
  e@(Op2 _ Lt e1 e2) -> case typeOf e1 of
    Bool   -> error $ "Comparing Bools: " ++ show e
    Real   -> (.<.) <$> transR e1    <*> transR e2
    BV8    -> bvult <$> transBV8 e1  <*> transBV8 e2
    BV16   -> bvult <$> transBV16 e1 <*> transBV16 e2
    BV32   -> bvult <$> transBV32 e1 <*> transBV32 e2
    BV64   -> bvult <$> transBV64 e1 <*> transBV64 e2
    SBV8   -> bvult <$> transBV8 e1  <*> transBV8 e2
    SBV16  -> bvult <$> transBV16 e1 <*> transBV16 e2
    SBV32  -> bvult <$> transBV32 e1 <*> transBV32 e2
    SBV64  -> bvult <$> transBV64 e1 <*> transBV64 e2
  e@(Op2 _ Gt e1 e2) -> case typeOf e1 of
    Bool   -> error $ "Comparing Bools: " ++ show e
    Real   -> (.>.) <$> transR e1    <*> transR e2
    BV8    -> bvugt <$> transBV8 e1  <*> transBV8 e2
    BV16   -> bvugt <$> transBV16 e1 <*> transBV16 e2
    BV32   -> bvugt <$> transBV32 e1 <*> transBV32 e2
    BV64   -> bvugt <$> transBV64 e1 <*> transBV64 e2
    SBV8   -> bvugt <$> transBV8 e1  <*> transBV8 e2
    SBV16  -> bvugt <$> transBV16 e1 <*> transBV16 e2
    SBV32  -> bvugt <$> transBV32 e1 <*> transBV32 e2
    SBV64  -> bvugt <$> transBV64 e1 <*> transBV64 e2
  SVal _ s i         -> getBoolVar $ ncVar s i
  e                  -> error $ "Encountered unhandled expression (Bool): " ++ show e

ncVar :: [Char] -> SeqIndex -> [Char]
ncVar s (Fixed i) = s ++ "_" ++ show i
ncVar s (Var   i) = s ++ "_n" ++ show i

transR :: Expr -> Trans (SMTExpr Rational)
transR = \case
  ConstR n         -> return $ constant $ toRational n
  Ite _ c e1 e2    -> ite <$> transB c <*> transR e1 <*> transR e2

  Op1 _ Neg e      -> app neg <$> transR e
  Op1 _ Abs e      -> app SMTAbs <$> transR e

  Op2 _ Add e1 e2  -> (\x y -> app plus [x, y]) <$> transR e1 <*> transR e2
  Op2 _ Sub e1 e2  -> (\x y -> app minus (x, y)) <$> transR e1 <*> transR e2
  Op2 _ Mul e1 e2  -> (\x y -> app mult [x, y]) <$> transR e1 <*> transR e2
  Op2 _ Fdiv e1 e2 -> divide <$> transR e1 <*> transR e2

  Op2 _ Pow e1 e2  -> do
    let pow = SMTBuiltIn "^" () :: SMTFunction (SMTExpr Rational, SMTExpr Rational) Rational
    (\x y -> app pow (x, y)) <$> transR e1 <*> transR e2

  SVal _ s i       -> getRatVar $ ncVar s i
  e                -> error $ "Encountered unhandled expression (Rat): " ++ show e

transBV8 :: Expr -> Trans (SMTExpr BV8)
transBV8 = \case
  ConstI _ n      -> return $ constant $ BitVector n
  Ite _ c e1 e2   -> ite <$> transB c <*> transBV8 e1 <*> transBV8 e2
  Op1 _ Abs e     -> abs <$> transBV8 e
  Op1 _ Neg e     -> negate <$> transBV8 e
  Op2 _ Add e1 e2 -> (+) <$> transBV8 e1 <*> transBV8 e2
  Op2 _ Sub e1 e2 -> (-) <$> transBV8 e1 <*> transBV8 e2
  Op2 _ Mul e1 e2 -> (*) <$> transBV8 e1 <*> transBV8 e2
  SVal _ s i      -> getBV8Var $ ncVar s i
  e               -> error $ "Encountered unhandled expression (BV8): " ++ show e

transBV16 :: Expr -> Trans (SMTExpr BV16)
transBV16 = \case
  ConstI _ n      -> return $ constant $ BitVector n
  Ite _ c e1 e2   -> ite <$> transB c <*> transBV16 e1 <*> transBV16 e2
  Op1 _ Abs e     -> abs <$> transBV16 e
  Op1 _ Neg e     -> negate <$> transBV16 e
  Op2 _ Add e1 e2 -> (+) <$> transBV16 e1 <*> transBV16 e2
  Op2 _ Sub e1 e2 -> (-) <$> transBV16 e1 <*> transBV16 e2
  Op2 _ Mul e1 e2 -> (*) <$> transBV16 e1 <*> transBV16 e2
  SVal _ s i      -> getBV16Var $ ncVar s i
  e               -> error $ "Encountered unhandled expression (BV16): " ++ show e

transBV32 :: Expr -> Trans (SMTExpr BV32)
transBV32 = \case
  ConstI _ n      -> return $ constant $ BitVector n
  Ite _ c e1 e2   -> ite <$> transB c <*> transBV32 e1 <*> transBV32 e2
  Op1 _ Abs e     -> abs <$> transBV32 e
  Op1 _ Neg e     -> negate <$> transBV32 e
  Op2 _ Add e1 e2 -> (+) <$> transBV32 e1 <*> transBV32 e2
  Op2 _ Sub e1 e2 -> (-) <$> transBV32 e1 <*> transBV32 e2
  Op2 _ Mul e1 e2 -> (*) <$> transBV32 e1 <*> transBV32 e2
  SVal _ s i      -> getBV32Var $ ncVar s i
  e               -> error $ "Encountered unhandled expression (BV32): " ++ show e

transBV64 :: Expr -> Trans (SMTExpr BV64)
transBV64 = \case
  ConstI _ n      -> return $ constant $ BitVector n
  Ite _ c e1 e2   -> ite <$> transB c <*> transBV64 e1 <*> transBV64 e2
  Op1 _ Abs e     -> abs <$> transBV64 e
  Op1 _ Neg e     -> negate <$> transBV64 e
  Op2 _ Add e1 e2 -> (+) <$> transBV64 e1 <*> transBV64 e2
  Op2 _ Sub e1 e2 -> (-) <$> transBV64 e1 <*> transBV64 e2
  Op2 _ Mul e1 e2 -> (*) <$> transBV64 e1 <*> transBV64 e2
  SVal _ s i      -> getBV64Var $ ncVar s i
  e               -> error $ "Encountered unhandled expression (BV64): " ++ show e

-----------------------------------------------------
-- Debug stuff from the the smtlib2 library github --
-----------------------------------------------------

namedDebugBackend :: String -> Bool -> b -> DebugBackend b
namedDebugBackend name mute b = DebugBackend b stderr (Just 0) (Just name) True mute

data DebugBackend b = DebugBackend
  { debugBackend  :: b
  , debugHandle   :: Handle
  , debugLines    :: Maybe Integer
  , debugPrefix   :: Maybe String
  , debugUseColor :: Bool
  , mute          :: Bool
  }

instance (SMTBackend b m,MonadIO m) => SMTBackend (DebugBackend b) m where
  smtGetNames b = smtGetNames (debugBackend b)
  smtNextName b = smtNextName (debugBackend b)
  smtHandle b req = do
    getName <- smtGetNames (debugBackend b)
    nxtName <- smtNextName (debugBackend b)
    (dts,b1) <- smtHandle (debugBackend b) SMTDeclaredDataTypes
    let rendering = renderSMTRequest nxtName getName dts req
    case debugPrefix b of
      Nothing -> return ()
      Just prf -> case rendering of
        Right "" -> return ()
        _ -> do
          when (debugUseColor b) $ liftIO $ hSetSGR (debugHandle b) [Reset,SetColor Foreground Dull Cyan]
          liftIO $ unless (mute b) $ hPutStr (debugHandle b) prf
    nline <- case rendering of
     Right "" -> return (debugLines b)
     _ -> do
       nline <- case debugLines b of
         Nothing -> return Nothing
         Just line -> do
           when (debugUseColor b) $ liftIO $ hSetSGR (debugHandle b) [Reset,SetColor Foreground Dull Red]
           let line_str = show line
               line_str_len = length line_str
               line_str' = replicate (4-line_str_len) ' '++line_str++" "
           liftIO $ unless (mute b) $ hPutStr (debugHandle b) line_str'
           return (Just (line+1))
       case rendering of
        Left l -> do
          when (debugUseColor b) $ liftIO $ hSetSGR (debugHandle b) [Reset,SetColor Foreground Dull Green]
          liftIO $ unless (mute b) $ hPutStrLn (debugHandle b) (show l)
        Right msg -> do
          when (debugUseColor b) $ liftIO $ hSetSGR (debugHandle b) [Reset,SetColor Foreground Dull White]
          liftIO $ unless (mute b) $ hPutStr (debugHandle b) $ unlines $ fmap (\xs -> ';':xs) (lines msg)
       return nline
    (resp,b2) <- smtHandle b1 req
    case renderSMTResponse getName dts req resp of
      Nothing -> return ()
      Just str -> do
        when (debugUseColor b) $ liftIO $ hSetSGR (debugHandle b) [Reset,SetColor Foreground Dull Blue]
        liftIO $ unless (mute b) $ hPutStrLn (debugHandle b) str
    when (debugUseColor b) $ liftIO $ hSetSGR (debugHandle b) [Reset]
    return (resp,b { debugBackend = b2, debugLines = nline })
