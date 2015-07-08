--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs, RankNTypes #-}

module Copilot.Kind.TransSys.Spec
  ( module Copilot.Kind.Misc.Operators
  , module Copilot.Kind.Misc.Type
  , module Copilot.Kind.Misc.Invariants
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

import Copilot.Kind.Misc.Type
import Copilot.Kind.Misc.Operators
import Copilot.Kind.Misc.Invariants

import Copilot.Kind.Misc.Utils

import qualified Data.List  as List
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Bimap as Bimap

--------------------------------------------------------------------------------

type NodeId = String
type PropId = String

data TransSys = TransSys
  { specNodes         :: [Node]
  , specTopNodeId     :: NodeId
  , specProps         :: Map PropId ExtVar }


data Node = Node
  { nodeId            :: NodeId
  , nodeDependencies  :: [NodeId]
  , nodeLocalVars     :: Map Var VarDescr
  , nodeImportedVars  :: Bimap Var ExtVar
  , nodeConstrs       :: [Expr Bool] }


data Var      =  Var {varName :: String}
  deriving (Eq, Show, Ord)

data ExtVar   =  ExtVar {extVarNode :: NodeId, extVarLocalPart :: Var }
  deriving (Eq, Ord)

data VarDescr = forall t . VarDescr
  { varType :: Type t
  , varDef  :: VarDef t }

data VarDef t = Pre t Var | Expr (Expr t) | Constrs [Expr Bool]

data Expr t where
  Const :: Type t -> t -> Expr t
  Ite   :: Type t -> Expr Bool -> Expr t -> Expr t -> Expr t
  Op1   :: Type t -> Op1 x t -> Expr x -> Expr t
  Op2   :: Type t -> Op2 x y t -> Expr x -> Expr y -> Expr t
  VarE  :: Type t -> Var -> Expr t

--------------------------------------------------------------------------------

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

transformExpr :: (forall a . Expr a -> Expr a) -> Expr t -> Expr t
transformExpr f = tre
  where
    tre :: forall t . Expr t -> Expr t
    tre (Ite t c e1 e2)   = f (Ite t (tre c) (tre e1) (tre e2))
    tre (Op1 t op e)      = f (Op1 t op (tre e))
    tre (Op2 t op e1 e2)  = f (Op2 t op (tre e1) (tre e2))
    tre e                 = f e

--------------------------------------------------------------------------------

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

  in Map.fold (Set.union . descrRhsVars) Set.empty (nodeLocalVars n)

nodeImportedExtVarsSet :: Node -> Set ExtVar
nodeImportedExtVarsSet = Map.keysSet . Bimap.toMapR . nodeImportedVars

nodeExportedExtVarsSet :: Node -> Set ExtVar
nodeExportedExtVarsSet n = Set.map (ExtVar $ nodeId n) (nodeLocalVarsSet n)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

specNodesIds :: TransSys -> Set NodeId
specNodesIds s = Set.fromList . map nodeId $ specNodes s

specDependenciesGraph :: TransSys -> Map NodeId [NodeId]
specDependenciesGraph s =
  Map.fromList [ (nodeId n, nodeDependencies n) | n <- specNodes s ]

specTopNode :: TransSys -> Node
specTopNode spec = fromJust $ List.find
  ((== specTopNodeId spec) . nodeId)
  (specNodes spec)

--------------------------------------------------------------------------------

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

isTopologicallySorted :: TransSys -> Bool
isTopologicallySorted spec =
  isJust $ foldM inspect Set.empty (specNodes spec)
  where inspect acc n = do
          guard $ Set.fromList (nodeDependencies n) `isSubsetOf` acc
          return . Set.insert (nodeId n) $ acc

--------------------------------------------------------------------------------

-- For debugging purposes
instance Show ExtVar where
  show (ExtVar n v) = "(" ++ n ++ " : " ++ show v ++ ")"

--------------------------------------------------------------------------------
