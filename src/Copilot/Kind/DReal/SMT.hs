--------------------------------------------------------------------------------

module Copilot.Kind.DReal.SMT
  ( Solver
  , startNewSolver
  , assume
  , entailed
  , exit
  , SatResult (..)
  , declVars
  ) where

import Copilot.Kind.IL
import Copilot.Kind.Misc.SExpr
import Copilot.Kind.Misc.Error as Err

import System.IO
import System.Process
import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe

import qualified Copilot.Kind.DReal.SMTLib as SMT

--------------------------------------------------------------------------------

data Solver = Solver
  { inh        :: Handle
  , outh       :: Handle
  , process    :: ProcessHandle
  , debugMode  :: Bool
  , solverName :: String }

data SatResult
  = Sat
  | Unsat
  | Unknown

--------------------------------------------------------------------------------

debug :: Bool -> Solver -> String -> IO ()
debug printName s str = when (debugMode s) $ do
  putStrLn $ (if printName then "<" ++ solverName s ++ ">  " else "") ++ str


send :: Solver -> SMT.Term -> IO ()
send s t = do
  hPutStr (inh s) (show t ++ "\n")
  debug True s (show t)
  hFlush (inh s)


receive :: Solver -> IO String
receive s = do
  answer <- hGetLine (outh s)
  debug False s answer
  return answer

--------------------------------------------------------------------------------

startNewSolver :: String -> [SeqDescr] -> [VarDescr] -> Bool -> IO Solver
startNewSolver name seqs vars dbgMode = do
  (i, o, e, p) <- runInteractiveProcess cmd opts Nothing Nothing
  hClose e
  let s = Solver i o p dbgMode name
  send s (SMT.setLogic logic)
  return s
  where cmd  = "dReal"
        opts = []
        logic = "QF_NRA"

exit :: Solver -> IO ()
exit s = do
  hClose (inh s)
  hClose (outh s)
  terminateProcess (process s)

--------------------------------------------------------------------------------

assume :: Solver -> [Constraint] -> IO ()
assume s cs = forM_ cs (send s . SMT.assert)

entailed :: Solver -> [Constraint] -> IO SatResult
entailed s cs = do
  --send s SMT.push
  case cs of
      [] -> assume s [Const Bool True]
      _ -> assume s $ [foldl (Op2 Bool Or) (Const Bool False) (map (Op1 Bool Not) cs)]
  send s SMT.checkSat
  hClose (inh s)

  res <- receive s >>= \case
    "sat"     -> return Sat
    "unsat"   -> return Unsat
    "unknown" -> return Unknown
    s         -> Err.fatal $ "Unknown dReal output : '" ++ s ++ "'"

  --send s SMT.pop
  return res

declVars :: Solver -> [VarDescr] -> IO ()
declVars s vars = forM_ vars $
  \(VarDescr {varName, varType}) -> send s (SMT.declFun varName varType)

--------------------------------------------------------------------------------

