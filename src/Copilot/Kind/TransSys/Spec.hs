--------------------------------------------------------------------------------

module Copilot.Kind.TransSys.Spec
  ( module Copilot.Kind.Misc.Operators
  , module Copilot.Kind.Misc.Type
  , Spec (..)
  , Node (..)
  , PropId
  , NodeId
  , Var (..)
  , LVar (..)
  , GVar (..)
  , LVarDef (..)
  , LVarDescr (..)
  , Expr (..)
  , mkGVar) where

import Copilot.Kind.Misc.Type
import Copilot.Kind.Misc.Operators

import Data.Map (Map)

--------------------------------------------------------------------------------

-- | A Copilot.Core specification where
-- |   * The Copilot types are abstracted with 'Bool' and 'Integer'
-- |   * Streams are turned into Lustre-like nodes
-- |   * A dependency graph is built for these nodes
-- |   * No more local variables
-- |   * Equations have depth at most 1

-- | The following features are not handled :
-- |   * Floating point numbers


data Spec = Spec
  { specNodes         :: [Node]
  , specTopNodeId     :: NodeId
  , specProps         :: Map PropId GVar
  , specAssertDeps    :: Map PropId [PropId] }

type PropId = String

data Node = Node
  { nodeId            :: NodeId
  , nodeDependencies  :: [NodeId]
  , nodeVars          :: Map LVar LVarDescr }
  
type NodeId = String

class Var v where varName :: v -> String

data LVar = LVar
  { lvarName   :: String
  } deriving (Eq, Ord)

data GVar = GVar
  { varNode   :: NodeId
  , localPart :: LVar
  } deriving (Eq)

data LVarDescr = forall t . LVarDescr
  { varType :: Type t
  , varDef  :: LVarDef t }

data LVarDef t = Pre t LVar | Ext GVar | Expr (Expr t)

data Expr t where
  Const  :: Type t -> t -> Expr t
  Ite    :: Type t -> Expr Bool -> Expr t -> Expr t -> Expr t
  Op1    :: Type t -> Op1 x t -> Expr x -> Expr t
  Op2    :: Type t -> Op2 x y t -> Expr x -> Expr y -> Expr t
  VarE   :: Type t -> LVar -> Expr t

--------------------------------------------------------------------------------

instance Var LVar where varName = lvarName
instance Var GVar where varName = varName . localPart

mkGVar node name = GVar node (LVar name)

--------------------------------------------------------------------------------
