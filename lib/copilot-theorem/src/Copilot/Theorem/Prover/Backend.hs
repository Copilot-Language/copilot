{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}

-- | Backend to  SMT solvers and theorem provers.
--
-- This module provides three definitions:
--
-- - A class ('SmtFormat') abstracting over the language used to communicate the
-- desired commands to an SMT solver or theorem prover.
--
-- - A class ('Backend') abstracting over the backend, which includes the name of
-- the executable, any options and flags necessary, and functions to parse the
-- results and close the communication.
--
-- - A type ('SatResult') representing a satisfiability result communicated by
-- the SMT solver or theorem prover.
module Copilot.Theorem.Prover.Backend (SmtFormat(..), Backend(..), SatResult(..)) where

import Copilot.Theorem.IL

import System.IO

-- | Format of SMT-Lib commands.
class Show a => SmtFormat a where
   push            :: a
   pop             :: a
   checkSat        :: a
   setLogic        :: String -> a
   declFun         :: String -> Type -> [Type] -> a
   assert          :: Expr -> a

-- | Backend to an SMT solver or theorem prover.
data Backend a = Backend
  { name            :: String
  , cmd             :: String
  , cmdOpts         :: [String]
  , inputTerminator :: Handle -> IO ()
  , incremental     :: Bool
  , logic           :: String
  , interpret       :: String -> Maybe SatResult
  }

-- | Satisfiability result communicated by the SMT solver or theorem prover.
data SatResult = Sat | Unsat | Unknown
