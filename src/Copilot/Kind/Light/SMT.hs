--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase, NamedFieldPuns, RankNTypes, ViewPatterns #-}

module Copilot.Kind.Light.SMT
  ( Solver
  , startNewSolver, assume, entailed, stop, declVars, updateVars
  ) where

import Copilot.Kind.IL

import Copilot.Kind.Light.Backend

import System.IO
import System.Process
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Applicative ((<$>))
import Data.Maybe
import Data.Set ((\\), fromList, Set, union, empty, elems)

--------------------------------------------------------------------------------

data Solver a = Solver
  { solverName :: String
  , inh        :: Handle
  , outh       :: Handle
  , process    :: ProcessHandle
  , debugMode  :: Bool
  , vars       :: Set VarDescr
  , backend    :: Backend a
  }

--------------------------------------------------------------------------------

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

startNewSolver :: SmtFormat a => String -> Bool -> Backend a -> IO (Solver a)
startNewSolver name dbgMode b = do
  (i, o, e, p) <- runInteractiveProcess (cmd b) (cmdOpts b) Nothing Nothing
  hClose e
  let s = Solver name i o p dbgMode empty b
  send s $ setLogic $ logic b
  return s

stop :: Solver a -> IO ()
stop s = do
  hClose $ inh s
  hClose $ outh s
  terminateProcess $ process s

--------------------------------------------------------------------------------

assume :: SmtFormat a => Solver a -> [Expr] -> IO ()
assume s cs = forM_ cs (send s . assert)

entailed :: SmtFormat a => Solver a -> [Expr] -> IO SatResult
entailed s cs = do
  when (incremental $ backend s) $ send s push
  case cs of
      []  -> putStrLn "Warning: no proposition to prove." >> assume s [ConstB True]
      _   -> assume s [foldl1 (Op2 Bool Or) (map (Op1 Bool Not) cs)]
  send s checkSat
  (inputTerminator $ backend s) (inh s)

  when (incremental $ backend s) $ send s pop
  receive s

declVars :: SmtFormat a => Solver a -> [VarDescr] -> IO ()
declVars s@(Solver { vars }) decls = do
  let newVars = elems $ fromList decls \\ vars
  forM_ newVars $ \(VarDescr {varName, varType, args}) ->
    send s $ declFun varName varType args

updateVars :: Solver a -> [VarDescr] -> Solver a
updateVars s@(Solver { vars }) newVars =
  s { vars = vars `union` fromList newVars }

--------------------------------------------------------------------------------

