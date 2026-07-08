{-# LANGUAGE Safe #-}

-- | Abstract syntax tree of Kind2 native input files.
--
-- This represents the native transition system input format supported by
-- Kind2 1.0 and newer (@--input_format native@), in which a file is a
-- sequence of @define-node@ forms, each declaring a state transition system
-- by means of an initial state predicate and a transition relation predicate.
-- The last node defined in a file is the top system analyzed by Kind2, and
-- the properties to check are attached to it.
--
-- The native input format is largely undocumented, and Kind2's user
-- documentation only covers its Lustre frontend. The best available
-- references for the native format are the parser in the Kind2 sources
-- ([nativeInput.ml]
-- (https://github.com/kind2-mc/kind2/blob/v3.0.0/src/nativeInput.ml)),
-- which includes a sketch of the grammar in a comment near the end of the
-- file) and the example file
-- [two_counters.kind2]
-- (https://github.com/kind2-mc/kind2/blob/develop/examples/two_counters.kind2).
module Copilot.Theorem.Kind2.AST where

-- | A file is a sequence of node definitions, together with a distinguished
-- top node and a series of propositions about the top node.
--
-- Kind2 analyzes the last node defined in a file as the top system, so the
-- top node is always printed after all the other nodes.
data File = File
  { fileNodes   :: [Node]  -- ^ Nodes other than the top node, in dependency
                           --   order (a node may only refer to nodes defined
                           --   before it).
  , fileTopNode :: Node    -- ^ The top node, which the propositions are
                           --   attached to.
  , fileProps   :: [Prop]  -- ^ Propositions about the top node.
  }

-- | A proposition is defined by a term.
data Prop = Prop
  { propName      :: String
  , propTerm      :: Term }

-- | A node definition.
data Node = Node
  { nodeId        :: String         -- ^ Identifier for the node.
  , nodeStateVars :: [StateVarDef]  -- ^ Variables identifying the states in
                                    -- the underlying state transition system.
  , nodeInit      :: Term           -- ^ Predicate that holds for initial
                                    -- states.
  , nodeTrans     :: Term           -- ^ Predicate that holds for two states,
                                    -- if there is a state transition between
                                    -- them.
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
