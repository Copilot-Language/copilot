{-# LANGUAGE RankNTypes #-}

module Copilot.Kind.Light.Backend (SmtFormat(..), Backend(..), SatResult(..)) where

import Copilot.Kind.Misc.Type
import Copilot.Kind.IL

import System.IO

class Show a => SmtFormat a where
   push            :: a
   pop             :: a
   checkSat        :: a
   setLogic        :: String -> a
   declFun         :: forall t. String -> Type t -> a
   assert          :: Constraint -> a

data Backend a = Backend
  { cmd             :: String
  , cmdOpts         :: [String]
  , inputTerminator :: Handle -> IO ()
  , incremental     :: Bool
  , logic           :: String
  , interpret       :: String -> Maybe SatResult
  }

data SatResult = Sat | Unsat | Unknown

