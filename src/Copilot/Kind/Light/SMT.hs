--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase, NamedFieldPuns #-}

module Copilot.Kind.Light.SMT
  ( Solver
  , Backend (..)
  , SatResult (..)
  , startNewSolver, assume, entailed, stop, declVars, updateVars
  , smtLib2, tptp
  ) where

import Copilot.Kind.IL
import Copilot.Kind.Misc.SExpr
import Copilot.Kind.Misc.Error as Err

import System.IO
import System.Process
import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe
import Data.Set ((\\), fromList, Set, union, empty, elems)

import qualified Copilot.Kind.Light.SMTLib as SMT

--------------------------------------------------------------------------------

data Solver = Solver
  { solverName :: String
  , inh        :: Handle
  , outh       :: Handle
  , process    :: ProcessHandle
  , debugMode  :: Bool
  , backend    :: Backend
  , vars       :: Set VarDescr
  }

data Backend = Backend
  { cmd             :: String
  , cmdOpts         :: [String]
  , logic           :: String
  , inputTerminator :: Handle -> IO ()
  , incremental     :: Bool
  , format          :: Format
  }

data Format = Format
smtLib2 = Format
tptp = Format

data SatResult
  = Sat
  | Unsat
  | Unknown

--------------------------------------------------------------------------------

debug :: Bool -> Solver -> String -> IO ()
debug printName s str = when (debugMode s) $
  putStrLn $ (if printName then "<" ++ solverName s ++ ">  " else "") ++ str

send :: Solver -> SMT.Term -> IO ()
send s t = do
  hPutStr (inh s) (show t ++ "\n")
  debug True s (show t)
  hFlush (inh s)

receive :: Solver -> IO SatResult
receive s = do
  eof <- hIsEOF $ outh s
  if eof then debug True s "<<received EOF>>" >> return Unknown else do
    result <- hGetLine (outh s)
    debug True s result
    case result of
      "Theorem" -> return Sat
      "sat"     -> return Sat
      "unsat"   -> return Unsat
      _         -> return Unknown

--------------------------------------------------------------------------------

startNewSolver :: String -> Bool -> Backend -> IO Solver
startNewSolver name dbgMode b = do
  (i, o, e, p) <- runInteractiveProcess (cmd b) (cmdOpts b) Nothing Nothing
  hClose e
  let s = Solver name i o p dbgMode b empty
  send s $ SMT.setLogic (logic b)
  return s

stop :: Solver -> IO ()
stop s = do
  hClose $ inh s
  hClose $ outh s
  terminateProcess $ process s

--------------------------------------------------------------------------------

assume :: Solver -> [Constraint] -> IO ()
assume s cs = forM_ cs (send s . SMT.assert)

entailed :: Solver -> [Constraint] -> IO SatResult
entailed s cs = do
  when (incremental $ backend s) $ send s SMT.push
  case cs of
      []  -> assume s [Const Bool True]
      _   -> assume s [foldl1 (Op2 Bool Or) (map (Op1 Bool Not) cs)]
  send s SMT.checkSat
  -- TODO(chathhorn): dReal doesn't respond unless we close the input stream.
  (inputTerminator $ backend s) (inh s)

  when (incremental $ backend s) $ send s SMT.pop
  receive s

declVars :: Solver -> [VarDescr] -> IO ()
declVars s@(Solver { vars }) decls = do
  let newVars = elems $ (fromList decls) \\ vars
  forM_ newVars $ \(VarDescr {varName, varType}) -> send s (SMT.declFun varName varType)

updateVars :: Solver -> [VarDescr] -> Solver
updateVars s@(Solver { vars }) newVars =
  s { vars = (vars `union` fromList newVars) }

--------------------------------------------------------------------------------

