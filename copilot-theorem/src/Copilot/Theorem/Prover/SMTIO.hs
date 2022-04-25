--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE Safe           #-}
{-# LANGUAGE ViewPatterns   #-}

-- | Communication with SMT solvers or theorem provers.
--
-- A solver is a running process defined by a 'Backend'.
module Copilot.Theorem.Prover.SMTIO
  ( Solver
  , startNewSolver, stop
  , assume, entailed, declVars
  ) where

import Copilot.Theorem.IL
import Copilot.Theorem.Prover.Backend

import System.IO
import System.Process
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Set ((\\), fromList, Set, union, empty, elems)

--------------------------------------------------------------------------------

-- | A connection with a running SMT solver or theorem prover.
data Solver a = Solver
  { solverName :: String
  , inh        :: Handle
  , outh       :: Handle
  , process    :: ProcessHandle
  , debugMode  :: Bool
  , vars       :: Set VarDescr
  , model      :: Set Expr
  , backend    :: Backend a
  }

--------------------------------------------------------------------------------

-- | Output a debugging message if debugging is enabled for the solver.
debug :: Bool -> Solver a -> String -> IO ()
debug printName s str = when (debugMode s) $
  putStrLn $ (if printName then "<" ++ solverName s ++ ">  " else "") ++ str

send :: Show a => Solver a -> a -> IO ()
send _ (show -> "") = return ()
send s (show -> a) = do
    hPutStr (inh s) $ a ++ "\n"
    debug True s a
    hFlush $ inh s

receive :: Solver a -> IO SatResult
receive s = fromJust <$> runMaybeT (msum $ repeat line)
  where
    line :: MaybeT IO SatResult
    line = do
      eof <- liftIO $ hIsEOF $ outh s
      if eof
        then liftIO (debug True s "[received: EOF]") >> return Unknown
        else do
          ln <- liftIO $ hGetLine $ outh s
          liftIO $ debug True s $ "[received: " ++ ln ++ "]"
          MaybeT $ return $ (interpret $ backend s) ln

--------------------------------------------------------------------------------

-- | Create a new solver implemented by the backend specified.
--
-- The error handle from the backend handle is immediately closed/discarded,
-- and the logic initialized as specifiied by the backend options.
startNewSolver :: SmtFormat a => String -> Bool -> Backend a -> IO (Solver a)
startNewSolver name dbgMode b = do
  (i, o, e, p) <- runInteractiveProcess (cmd b) (cmdOpts b) Nothing Nothing
  hClose e
  let s = Solver name i o p dbgMode empty empty b
  send s $ setLogic $ logic b
  return s

-- | Stop a solver, closing all communication handles and terminating the
-- process.
stop :: Solver a -> IO ()
stop s = do
  hClose $ inh s
  hClose $ outh s
  terminateProcess $ process s

--------------------------------------------------------------------------------

-- | Register the given expressions as assumptions or axioms with the solver.
assume :: SmtFormat a => Solver a -> [Expr] -> IO (Solver a)
assume s@(Solver { model }) cs = do
  let newAxioms = elems $ fromList cs \\ model
  assume' s newAxioms
  return s { model = model `union` fromList newAxioms }

assume' :: SmtFormat a => Solver a -> [Expr] -> IO ()
assume' s cs = forM_ cs (send s . assert . bsimpl)

-- | Check if a series of expressions are entailed by the axioms or assumptions
-- already registered with the solver.
entailed :: SmtFormat a => Solver a -> [Expr] -> IO SatResult
entailed s cs = do
  when (incremental $ backend s) $ send s push
  case cs of
      []  -> putStrLn "Warning: no proposition to prove." >> assume' s [ConstB True]
      _   -> assume' s [foldl1 (Op2 Bool Or) (map (Op1 Bool Not) cs)]
  send s checkSat
  (inputTerminator $ backend s) (inh s)

  when (incremental $ backend s) $ send s pop
  receive s

-- | Register the given variables with the solver.
declVars :: SmtFormat a => Solver a -> [VarDescr] -> IO (Solver a)
declVars s@(Solver { vars }) decls = do
  let newVars = elems $ fromList decls \\ vars
  forM_ newVars $ \(VarDescr {varName, varType, args}) ->
    send s $ declFun varName varType args
  return s { vars = vars `union` fromList newVars }

--------------------------------------------------------------------------------
