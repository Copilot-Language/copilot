{-# LANGUAGE RankNTypes #-}

module Copilot.Theorem.Prover.Backend (SmtFormat(..), Backend(..), SatResult(..)) where

import Copilot.Theorem.IL

import System.IO

class Show a => SmtFormat a where
   push            :: a
   pop             :: a
   checkSat        :: a
   setLogic        :: String -> a
   declFun         :: String -> Type -> [Type] -> a
   assert          :: Expr -> a

data Backend a = Backend
  { name            :: String
  , cmd             :: String
  , cmdOpts         :: [String]
  , inputTerminator :: Handle -> IO ()
  , incremental     :: Bool
  , logic           :: String
  , interpret       :: String -> Maybe SatResult
  }

data SatResult = Sat | Unsat | Unknown

