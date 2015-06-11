--------------------------------------------------------------------------------

module Copilot.Kind.Kind2.AST where

--------------------------------------------------------------------------------

data File = File
  { filePreds     :: [PredDef]
  , fileProps     :: [Prop] }

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

data Type = Int | Real | Bool

data StateVarFlag = FConst

data PredType = Init | Trans

data Term =
    ValueLitteral  String
  | PrimedStateVar String
  | StateVar       String
  | FunApp         String [Term]
  | PredApp        String PredType [Term]

--------------------------------------------------------------------------------
