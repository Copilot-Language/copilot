--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase, NamedFieldPuns, FlexibleInstances, RankNTypes, GADTs,
    MultiParamTypeClasses #-}

module Copilot.Kind.Light.Z3Prover
  ( module Data.Default
  , Options (..)
  , kInduction, onlySat
  )
where

import Copilot.Kind.IL.Translate
import Copilot.Kind.IL
import Copilot.Kind.Prover (Output (..))
import qualified Copilot.Kind.Prover as P

import Control.Applicative ((<$>), (<*))
import Control.Monad (msum, mzero, when, void)
import Control.Monad.State (StateT, runStateT, get, modify)
import Control.Monad.Trans.Maybe (MaybeT (..))

import Data.Word
import Data.Unit
import Data.Maybe (fromJust, fromMaybe)
import Data.Function (on)
import Data.Default (Default(..))
import Data.List (foldl')

import Data.Set (Set, (\\), union)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Copilot.Kind.Misc.Utils

import Language.SMTLib2
import Language.SMTLib2.Pipe
import Language.SMTLib2.Connection
import Language.SMTLib2.Strategy

import Language.SMTLib2.Internals hiding (Var)

import System.Console.ANSI
import System.IO
import Control.Monad.Trans

--------------------------------------------------------------------------------

type Solver = SMTConnection (DebugBackend SMTPipe)

data Options = Options
  -- The maximum number of steps of the k-induction algorithm the prover runs
  -- before giving up.
  { maxK :: Word32

  -- If `debug` is set to `True`, the SMTLib/TPTP queries produced by the
  -- prover are displayed in the standard output.
  , debug   :: Bool
  }

instance Default Options where
  def = Options
    { maxK  = 10
    , debug = False
    }

onlySat :: Options -> P.Prover
onlySat options = P.Prover
  { P.proverName  = "OnlySat"
  , P.hasFeature  = \case
      P.GiveCex -> False
      P.HandleAssumptions -> True
  , P.startProver = return . ProofState options Map.empty Map.empty Map.empty . translate
  , P.askProver   = onlySat'
  , P.closeProver = const $ return ()
  }

kInduction :: Options -> P.Prover
kInduction options = P.Prover
  { P.proverName  = "k-induction"
  , P.hasFeature  = \case
      P.GiveCex -> False
      P.HandleAssumptions -> True
  , P.startProver = return . ProofState options Map.empty Map.empty Map.empty . translate
  , P.askProver   = kInduction' (maxK options)
  , P.closeProver = const $ return ()
  }

-------------------------------------------------------------------------------

-- | Checks the Copilot specification with k-induction

type ProofScript = MaybeT (StateT ProofState IO)

runPS :: ProofScript a -> ProofState -> IO (Maybe a, ProofState)
runPS ps = runStateT (runMaybeT ps)

data ProofState = ProofState
  { options :: Options
  , solvers :: Map SolverId Solver
  , vars    :: Map SolverId TransState
  , assumps :: Map SolverId (Set Expr)
  , spec    :: IL
  }

data SolverId = Base | Step
  deriving (Show, Ord, Eq)

getModels :: [PropId] -> [PropId] -> ProofScript ([Expr], [Expr], [Expr], Bool)
getModels assumptionIds toCheckIds = do
  IL {modelInit, modelRec, properties, inductive} <- spec <$> get
  let (as, as')       = selectProps assumptionIds properties
      (as'', toCheck) = selectProps toCheckIds properties
      modelRec'       = modelRec ++ as ++ as' ++ as''
  return (modelInit, modelRec', toCheck, inductive)

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
  s <- liftIO $ open (debugBackend pipe)
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
  -- liftIO $ performSMT s $ setOption (ProduceModels True)
  -- liftIO $ performSMT s $ setOption (ProduceProofs True)
  -- liftIO $ performSMT s $ setOption (ProduceUnsatCores True)
  vs <- getVars sid
  assumps' <- newAssumps sid assumptions
  (_, vs')  <- liftIO $ performSMT s $ runStateT (mapM_ (\e -> transB e >>= lift . assert) assumps') vs
  setVars sid vs'
  liftIO $ performSMT s $ push
  (_, vs'') <- liftIO $ performSMT s $ runStateT
    (transB (foldl' (Op2 Bool Or) (ConstB False) $ map (Op1 Bool Not) props) >>= lift . assert) vs'

  res <- if hasSomeReal props
    then liftIO $ performSMT s $ checkSat' Nothing (CheckSatLimits (Just 5000) Nothing)
    else liftIO $ performSMT s $ checkSat' (Just (UsingParams (CustomTactic "qfnra-nlsat") []))
         (CheckSatLimits (Just 5000) Nothing)

  when (res == Sat) $ void $ liftIO $ performSMT s $ getModel
  liftIO $ performSMT s $ pop
  -- liftIO $ print model
  return res

-- TODO
hasSomeReal _ = False

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

kInduction' :: Word32 -> ProofState -> [PropId] -> [PropId] -> IO Output
kInduction' k s as ps = (fromMaybe (Output P.Unknown ["proof by k-induction failed"]) . fst)
  <$> runPS (msum (map induction [0..toInteger k]) <* stopSolvers) s
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

onlySat' :: ProofState -> [PropId] -> [PropId] -> IO Output
onlySat' s as ps = (fromJust . fst) <$> runPS (script <* stopSolvers) s
  where
    script  = do
      (modelInit, modelRec, toCheck, inductive) <- getModels as ps

      let base    = map (evalAt (Fixed 0)) modelRec
          baseInv = map (evalAt (Fixed 0)) toCheck

      when inductive $ liftIO $
        putStrLn "Warning: OnlySat doesn't really work with induction."

      entailment Base (modelInit ++ base) (map (Op1 Bool Not) baseInv) >>= \case
        Unsat   -> invalid "prop not satisfiable"
        Unknown -> unknown' "failed to find a satisfying model"
        Sat     -> sat "prop is satisfiable"

selectProps :: [PropId] -> Map PropId ([Expr], Expr) -> ([Expr], [Expr])
selectProps propIds properties =
  (squash . unzip) [(as, p) | (id, (as, p)) <- Map.toList properties, id `elem` propIds]
    where squash (a, b) = (concat a, b)

--------------------------------------------------------------------------------

type Trans = StateT TransState SMT

data TransState = TransState
  { intVars :: Map String (SMTExpr Integer)
  , ratVars :: Map String (SMTExpr Rational)
  }

noVars = TransState Map.empty Map.empty

getIntVar v = do
  vs <- intVars <$> get
  case Map.lookup v vs of
    Nothing -> do
      newVar <- lift $ varNamed v
      modify (\s -> s {intVars = Map.insert v newVar (intVars s)})
      return newVar
    Just x -> return x

getRatVar v = do
  vs <- ratVars <$> get
  case Map.lookup v vs of
    Nothing -> do
      newVar <- lift $ varNamed v
      modify (\s -> s {ratVars = Map.insert v newVar (ratVars s)})
      return newVar
    Just x -> return x

transB :: Expr -> Trans (SMTExpr Bool)
transB = \case
  ConstB b -> return $ constant b

  Ite _ c e1 e2   -> do
    c' <- transB c
    trans2B e1 e2 (ite c')

  Op1 _ Not e     -> transB e >>=  return . not'

  Op2 _ And e1 e2 -> do
    e1' <- transB e1
    e2' <- transB e2
    return $ e1' .&&. e2'
  Op2 _ Or e1 e2 -> do
    e1' <- transB e1
    e2' <- transB e2
    return $ e1' .||. e2'

  Op2 _ Eq e1 e2 -> case typeOf e1 of
    Integer -> trans2I e1 e2 (.==.)
    Real -> trans2R e1 e2 (.==.)
    Bool -> trans2B e1 e2 (.==.)

  Op2 _ Le e1 e2 -> case typeOf e1 of
    Integer -> trans2I e1 e2 (.<=.)
    Real -> trans2R e1 e2 (.<=.)
    _ -> undefined
  Op2 _ Ge e1 e2 -> case typeOf e1 of
    Integer -> trans2I e1 e2 (.>=.)
    Real -> trans2R e1 e2 (.>=.)
    _ -> undefined
  Op2 _ Lt e1 e2 -> case typeOf e1 of
    Integer -> trans2I e1 e2 (.<.)
    Real -> trans2R e1 e2 (.<.)
    _ -> undefined
  Op2 _ Gt e1 e2 -> case typeOf e1 of
    Integer -> trans2I e1 e2 (.>.)
    Real -> trans2R e1 e2 (.>.)
    _ -> undefined

  e -> error $ "Encountered unhandled expression:" ++ show e

ncVar s (Fixed i) = s ++ "_" ++ show i
ncVar s (Var   i) = s ++ "_n" ++ show i

transR :: Expr -> Trans (SMTExpr Rational)
transR = \case
  ConstR n -> return $ constant $ toRational n
  Ite _ c e1 e2   -> do
    c' <- transB c
    trans2R e1 e2 (ite c')

  Op1 _ Neg e     -> transR e >>=  return . (app neg)
  Op1 _ Abs e     -> transR e >>= return . (app SMTAbs)

  Op2 _ Add e1 e2 -> trans2R e1 e2 $ \x y -> app plus [x, y]
  Op2 _ Sub e1 e2 -> trans2R e1 e2 $ \x y -> app minus (x, y)
  Op2 _ Mul e1 e2 -> trans2R e1 e2 $ \x y -> app mult [x, y]
  Op2 _ Fdiv e1 e2 -> trans2R e1 e2 divide

  Op2 _ Pow e1 e2 -> do
    let pow = SMTBuiltIn "^" unit :: SMTFunction (SMTExpr Rational, SMTExpr Rational) Rational
    trans2R e1 e2 $ \x y -> app pow (x, y)

  SVal _ s i -> getRatVar $ ncVar s i

  e -> error $ "Encountered unhandled expression:" ++ show e

transI :: Expr -> Trans (SMTExpr Integer)
transI = \case
  ConstI n -> return $ constant n
  Ite _ c e1 e2   -> do
    c' <- transB c
    trans2I e1 e2 (ite c')

  Op1 _ Neg e     -> transI e >>=  return . (app neg)
  Op1 _ Abs e     -> transI e >>= return . (app SMTAbs)

  Op2 _ Add e1 e2 -> trans2I e1 e2 $ \x y -> app plus [x, y]
  Op2 _ Sub e1 e2 -> trans2I e1 e2 $ \x y -> app minus (x, y)
  Op2 _ Mul e1 e2 -> trans2I e1 e2 $ \x y -> app mult [x, y]

  SVal _ s i -> getIntVar $ ncVar s i

  e -> error $ "Encountered unhandled expression:" ++ show e

trans2I :: Expr -> Expr -> (SMTExpr Integer -> SMTExpr Integer -> SMTExpr b) -> Trans (SMTExpr b)
trans2I e1 e2 f = do
  e1' <- transI e1
  e2' <- transI e2
  return $ f e1' e2'

trans2R :: Expr -> Expr -> (SMTExpr Rational -> SMTExpr Rational -> SMTExpr b) -> Trans (SMTExpr b)
trans2R e1 e2 f = do
  e1' <- transR e1
  e2' <- transR e2
  return $ f e1' e2'

trans2B :: Expr -> Expr -> (SMTExpr Bool -> SMTExpr Bool -> SMTExpr b) -> Trans (SMTExpr b)
trans2B e1 e2 f = do
  e1' <- transB e1
  e2' <- transB e2
  return $ f e1' e2'

-----------------------------------------------------
-- Debug stuff from the the smtlib2 library github --
-----------------------------------------------------

debugBackend :: b -> DebugBackend b
debugBackend b = DebugBackend b stderr (Just 0) Nothing True

namedDebugBackend :: String -> b -> DebugBackend b
namedDebugBackend name b = DebugBackend b stderr (Just 0) (Just name) True

data DebugBackend b = DebugBackend { debugBackend' :: b
                                   , debugHandle :: Handle
                                   , debugLines :: Maybe Integer
                                   , debugPrefix :: Maybe String
                                   , debugUseColor :: Bool
                                   }

instance (SMTBackend b m,MonadIO m) => SMTBackend (DebugBackend b) m where
  smtGetNames b = smtGetNames (debugBackend' b)
  smtNextName b = smtNextName (debugBackend' b)
  smtHandle b req = do
    getName <- smtGetNames (debugBackend' b)
    nxtName <- smtNextName (debugBackend' b)
    (dts,b1) <- smtHandle (debugBackend' b) SMTDeclaredDataTypes
    let rendering = renderSMTRequest nxtName getName dts req
    case debugPrefix b of
      Nothing -> return ()
      Just prf -> case rendering of
        Right "" -> return ()
        _ -> do
          when (debugUseColor b) $ liftIO $ hSetSGR (debugHandle b) [Reset,SetColor Foreground Dull Cyan]
          liftIO $ hPutStr (debugHandle b) prf
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
           liftIO $ hPutStr (debugHandle b) line_str'
           return (Just (line+1))
       case rendering of
        Left l -> do
          when (debugUseColor b) $ liftIO $ hSetSGR (debugHandle b) [Reset,SetColor Foreground Dull Green]
          liftIO $ hPutStrLn (debugHandle b) (show l)
        Right msg -> do
          when (debugUseColor b) $ liftIO $ hSetSGR (debugHandle b) [Reset,SetColor Foreground Dull White]
          liftIO $ hPutStr (debugHandle b) $ unlines $ fmap (\xs -> ';':xs) (lines msg)
       return nline
    (resp,b2) <- smtHandle b1 req
    case renderSMTResponse getName dts req resp of
      Nothing -> return ()
      Just str -> do
        when (debugUseColor b) $ liftIO $ hSetSGR (debugHandle b) [Reset,SetColor Foreground Dull Blue]
        liftIO $ hPutStrLn (debugHandle b) str
    when (debugUseColor b) $ liftIO $ hSetSGR (debugHandle b) [Reset]
    return (resp,b { debugBackend' = b2 , debugLines = nline })


