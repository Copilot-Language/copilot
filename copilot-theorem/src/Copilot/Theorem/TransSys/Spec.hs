{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE Safe                      #-}

-- | Specification of Copilot streams as modular transition systems.
module Copilot.Theorem.TransSys.Spec
  ( module Copilot.Theorem.TransSys.Operators
  , module Copilot.Theorem.TransSys.Type
  , module Copilot.Theorem.TransSys.Invariants
  , TransSys (..)
  , Node (..)
  , PropId
  , NodeId
  , Var (..)
  , ExtVar (..)
  , VarDef (..)
  , VarDescr (..)
  , Expr (..)
  , mkExtVar
  , transformExpr
  , isTopologicallySorted
  , nodeVarsSet
  , specDependenciesGraph
  , specTopNode ) where

import Copilot.Theorem.TransSys.Type
import Copilot.Theorem.TransSys.Operators
import Copilot.Theorem.TransSys.Invariants

import Copilot.Theorem.Misc.Utils

import Control.Applicative (liftA2)
import Control.Monad (foldM, guard)

import Data.Maybe
import Data.Monoid ((<>))
import Data.Map (Map)
import Data.Set (Set, isSubsetOf, member)
import Data.Bimap (Bimap)

import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Bimap as Bimap

-- | Unique name that identifies a node.
type NodeId = String

-- | Unique name that identifies a property.
type PropId = String

-- | A modular transition system is defined by a graph of nodes and a series
-- of properties, each mapped to a variable.
data TransSys = TransSys
  { specNodes         :: [Node]
  , specTopNodeId     :: NodeId
  , specProps         :: Map PropId ExtVar }

-- | A node is a set of variables living in a local namespace and corresponding
-- to the 'Var' type.
data Node = Node
  { nodeId            :: NodeId
  , nodeDependencies  :: [NodeId]          -- ^ Nodes from which variables are
                                           --   imported.
  , nodeLocalVars     :: Map Var VarDescr  -- ^ Locally defined variables,
                                           --   either as the previous value of
                                           --   another variable (using 'Pre'),
                                           --   an expression involving
                                           --   variables (using 'Expr') or a
                                           --   set of constraints (using
                                           --   'Constrs').
  , nodeImportedVars  :: Bimap Var ExtVar  -- ^ Binds each imported variable to
                                           --   its local name.
  , nodeConstrs       :: [Expr Bool] }

-- | Identifer of a variable in the local (within one node) namespace.
data Var      =  Var {varName :: String}
  deriving (Eq, Show, Ord)

-- | Identifer of a variable in the global namespace by specifying both a node
-- name and a variable.
data ExtVar   =  ExtVar {extVarNode :: NodeId, extVarLocalPart :: Var }
  deriving (Eq, Ord)

-- | A description of a variable together with its type.
data VarDescr = forall t . VarDescr
  { varType :: Type t
  , varDef  :: VarDef t }

-- | A variable definition either as a delay, an operation on variables, or
-- a constraint.
data VarDef t = Pre t Var | Expr (Expr t) | Constrs [Expr Bool]

-- | A point-wise (time-wise) expression.
data Expr t where
  Const :: Type t -> t -> Expr t
  Ite   :: Type t -> Expr Bool -> Expr t -> Expr t -> Expr t
  Op1   :: Type t -> Op1 t -> Expr t -> Expr t
  Op2   :: Type t -> Op2 a t -> Expr a -> Expr a -> Expr t
  VarE  :: Type t -> Var -> Expr t

-- | Constructor for variables identifiers in the global namespace.
mkExtVar node name = ExtVar node (Var name)

foldExpr :: (Monoid m) => (forall t . Expr t -> m) -> Expr a -> m
foldExpr f expr = f expr <> fargs
  where
    fargs = case expr of
      (Ite _ c e1 e2)  -> foldExpr f c <> foldExpr f e1 <> foldExpr f e2
      (Op1 _ _ e)      -> foldExpr f e
      (Op2 _ _ e1 e2)  -> foldExpr f e1 <> foldExpr f e2
      _                -> mempty

foldUExpr :: (Monoid m) => (forall t . Expr t -> m) -> U Expr -> m
foldUExpr f (U e) = foldExpr f e

-- | Apply an arbitrary transformation to the leafs of an expression.
transformExpr :: (forall a . Expr a -> Expr a) -> Expr t -> Expr t
transformExpr f = tre
  where
    tre :: forall t . Expr t -> Expr t
    tre (Ite t c e1 e2)   = f (Ite t (tre c) (tre e1) (tre e2))
    tre (Op1 t op e)      = f (Op1 t op (tre e))
    tre (Op2 t op e1 e2)  = f (Op2 t op (tre e1) (tre e2))
    tre e                 = f e

-- | The set of variables related to a node (union of the local variables and
-- the imported variables after deferencing them).
nodeVarsSet :: Node -> Set Var
nodeVarsSet = liftA2 Set.union
  nodeLocalVarsSet
  (Map.keysSet . Bimap.toMap  . nodeImportedVars)

nodeLocalVarsSet :: Node -> Set Var
nodeLocalVarsSet = Map.keysSet . nodeLocalVars

nodeRhsVarsSet :: Node -> Set Var
nodeRhsVarsSet n =
  let varOcc (VarE _ v) = Set.singleton v
      varOcc _          = Set.empty

      descrRhsVars (VarDescr _ (Expr e))      = foldExpr varOcc e
      descrRhsVars (VarDescr _ (Pre _ v))     = Set.singleton v
      descrRhsVars (VarDescr _ (Constrs cs))  =
        mconcat (map (foldExpr varOcc) cs)

  in Map.foldr (Set.union . descrRhsVars) Set.empty (nodeLocalVars n)

nodeImportedExtVarsSet :: Node -> Set ExtVar
nodeImportedExtVarsSet = Map.keysSet . Bimap.toMapR . nodeImportedVars

nodeExportedExtVarsSet :: Node -> Set ExtVar
nodeExportedExtVarsSet n = Set.map (ExtVar $ nodeId n) (nodeLocalVarsSet n)

instance HasInvariants Node where

  invariants n =
    [ prop "The dependencies declaration doesn't lie" $
      (map extVarNode . Bimap.elems $ nodeImportedVars n)
      `isSublistOf` nodeDependencies n

    , prop "All local variables are declared" $
      nodeRhsVarsSet n `isSubsetOf` nodeVarsSet n

    , prop "Never apply 'pre' to an imported var" $
      let preVars = Set.fromList
            [v | (VarDescr _ (Pre _ v)) <- Map.elems $ nodeLocalVars n]
      in preVars `isSubsetOf` nodeLocalVarsSet n
    ]

specNodesIds :: TransSys -> Set NodeId
specNodesIds s = Set.fromList . map nodeId $ specNodes s

-- | Given a modular transition system, produce a map from each node to its
-- dependencies.
specDependenciesGraph :: TransSys -> Map NodeId [NodeId]
specDependenciesGraph s =
  Map.fromList [ (nodeId n, nodeDependencies n) | n <- specNodes s ]

-- | Return the top node of a modular transition system.
specTopNode :: TransSys -> Node
specTopNode spec = fromJust $ List.find
  ((== specTopNodeId spec) . nodeId)
  (specNodes spec)

instance HasInvariants TransSys where

  invariants s =
    [ prop "All mentioned nodes are declared" $
      specTopNodeId s `member` specNodesIds s
      && Set.fromList [nId | n <- specNodes s, nId <- nodeDependencies n]
         `isSubsetOf` specNodesIds s

    , prop "The imported vars are not broken" $
      mconcat (map nodeImportedExtVarsSet $ specNodes s) `isSubsetOf`
      mconcat (map nodeExportedExtVarsSet $ specNodes s)

    , prop "The nodes invariants hold" $ all checkInvs (specNodes s)
    ]

-- | True if the graph is topologically sorted (i.e., if the dependencies of a
-- node appear in the list of 'specNodes' before the node that depends on
-- them).
isTopologicallySorted :: TransSys -> Bool
isTopologicallySorted spec =
  isJust $ foldM inspect Set.empty (specNodes spec)
  where
    inspect acc n = do
      guard $ Set.fromList (nodeDependencies n) `isSubsetOf` acc
      return . Set.insert (nodeId n) $ acc

-- For debugging purposes
instance Show ExtVar where
  show (ExtVar n v) = "(" ++ n ++ " : " ++ show v ++ ")"
