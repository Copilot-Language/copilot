--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

-- | Abstract syntax tree of Kind2 files.
module Copilot.Theorem.Kind2.AST where

--------------------------------------------------------------------------------

-- | A file is a sequence of predicates and propositions.
data File = File
  { filePreds     :: [PredDef]
  , fileProps     :: [Prop] }

-- | A proposition is defined by a term.
data Prop = Prop
  { propName      :: String
  , propTerm      :: Term }

data PredDef = PredDef
  { predId        :: String
  , predStateVars :: [StateVarDef]
  , predInit      :: Term
  , predTrans     :: Term }

data StateVarDef = StateVarDef
  { varId         :: String
  , varType       :: Type
  , varFlags      :: [StateVarFlag] }

-- | Types used in Kind2 files to represent Copilot types.
--
-- The Kind2 backend provides functions to, additionally, constrain the range
-- of numeric values depending on their Copilot type ('Int8', 'Int16', etc.).
data Type = Int | Real | Bool

data StateVarFlag = FConst

data PredType = Init | Trans

data Term =
    ValueLiteral  String
  | PrimedStateVar String
  | StateVar       String
  | FunApp         String [Term]
  | PredApp        String PredType [Term]

--------------------------------------------------------------------------------
