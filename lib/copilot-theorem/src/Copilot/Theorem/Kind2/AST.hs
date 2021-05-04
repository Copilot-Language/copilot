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

-- | A predicate definition.
data PredDef = PredDef
  { predId        :: String         -- ^ Identifier for the predicate.
  , predStateVars :: [StateVarDef]  -- ^ Variables identifying the states in the
                                    -- underlying state transition system.
  , predInit      :: Term           -- ^ Predicate that holds for initial
                                    -- states.
  , predTrans     :: Term           -- ^ Predicate that holds for two states, if
                                    -- there is state transition between them.
  }

-- | A definition of a state variable.
data StateVarDef = StateVarDef
  { varId         :: String           -- ^ Name of the variable.
  , varType       :: Type             -- ^ Type of the variable.
  , varFlags      :: [StateVarFlag] } -- ^ Flags for the variable.

-- | Types used in Kind2 files to represent Copilot types.
--
-- The Kind2 backend provides functions to, additionally, constrain the range
-- of numeric values depending on their Copilot type ('Int8', 'Int16', etc.).
data Type = Int | Real | Bool

-- | Possible flags for a state variable.
data StateVarFlag = FConst

-- | Type of the predicate, either belonging to an initial state or a pair of
-- states with a transition.
data PredType = Init | Trans

-- | Datatype to describe a term in the Kind language.
data Term =
    ValueLiteral  String
  | PrimedStateVar String
  | StateVar       String
  | FunApp         String [Term]
  | PredApp        String PredType [Term]

--------------------------------------------------------------------------------
