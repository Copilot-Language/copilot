--------------------------------------------------------------------------------

module Copilot.Kind.TransSys.Spec
  ( module Copilot.Kind.Misc.Operators
  , module Copilot.Kind.Misc.Type
  , module Copilot.Kind.Misc.Invariants
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
  , mkGVar
  , transformExpr) where

import Copilot.Kind.Misc.Type
import Copilot.Kind.Misc.Operators
import Copilot.Kind.Misc.Invariants

import Data.Map (Map)
import Data.Bimap (Bimap)

import Copilot.Kind.Misc.Utils
import Data.Set (isSubsetOf, member)

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Bimap as Bimap

--------------------------------------------------------------------------------

-- | A Copilot.Core specification where
-- |   * The Copilot types are abstracted with 'Bool' and 'Integer'
-- |   * Streams are turned into Lustre-like nodes
-- |   * A dependency graph is built for these nodes
-- |   * No more local variables
-- |   * Equations have depth at most 1

-- | The following features are not handled :
-- |   * Floating point numbers

--------------------------------------------------------------------------------


data Spec = Spec
  { specNodes         :: [Node]
  , specTopNodeId     :: NodeId
  , specProps         :: Map PropId GVar
  , specAssertDeps    :: Map PropId [PropId] }

type PropId = String

data Node = Node
  { nodeId            :: NodeId
  , nodeDependencies  :: [NodeId]
  , nodeVars          :: Map LVar LVarDescr 
  , nodeImportedVars  :: Bimap LVar GVar }

type NodeId = String

class Var v where varName :: v -> String

data LVar = LVar
  { lvarName   :: String
  } deriving (Eq, Ord)

data GVar = GVar
  { varNode   :: NodeId
  , localPart :: LVar
  } deriving (Eq, Ord)

data LVarDescr = forall t . LVarDescr
  { varType :: Type t
  , varDef  :: LVarDef t }

data LVarDef t = Imported | Pre t LVar | Expr (Expr t)

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

foldExpr :: (Monoid m) => (forall t . Expr t -> m) -> (Expr a) -> m
foldExpr f expr = f expr <> fargs 
  where
    fargs = case expr of 
      (Ite _ c e1 e2)  -> foldExpr f c <> foldExpr f e1 <> foldExpr f e2
      (Op1 _ _ e)      -> foldExpr f e
      (Op2 _ _ e1 e2)  -> foldExpr f e1 <> foldExpr f e2
      _                -> mempty
      
foldUExpr :: (Monoid m) => (forall t . Expr t -> m) -> (U Expr) -> m
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

nodeLhsLVars :: Node -> Set LVar
nodeLhsLVars = Set.fromList . Map.keys . nodeVars
              
nodeExprs :: Node -> [U Expr]
nodeExprs n = mapMaybe varDescrExpr (Map.elems $ nodeVars n)
  where
    varDescrExpr (LVarDescr _ (Expr e)) = Just $ U e
    varDescrExpr _                      = Nothing
    
        
nodeRhsLVars :: Node -> Set LVar
nodeRhsLVars n = 
  let varOcc (VarE _ v) = Set.singleton v
      varOcc _          = Set.empty
      
      descrRhsVars (LVarDescr _ (Expr e))   = foldExpr varOcc e
      descrRhsVars (LVarDescr _ (Pre _ v))  = Set.singleton v
      descrRhsVars (LVarDescr _ (Imported)) = Set.empty
      
  in Map.fold (Set.union . descrRhsVars) Set.empty (nodeVars n)

nodeExportedGVars :: Node -> Set GVar
nodeExportedGVars n = Set.map (GVar (nodeId n)) (nodeLhsLVars n)

nodeImportedGVars :: Node -> Set GVar
nodeImportedGVars n = Set.fromList $ Bimap.elems (nodeImportedVars n)

nodeNonImportedLVars :: Node -> Set LVar
nodeNonImportedLVars n = Set.fromList $ 
  [ v | (v, d) <- Map.toList (nodeVars n), not (isImported d) ]

  where isImported (LVarDescr _ Imported) = True
        isImported _                      = False

--------------------------------------------------------------------------------

instance HasInvariants Node where

  invariants n =
    [ prop "The dependencies declaration doesn't lie" $
      (map varNode . Bimap.elems $ nodeImportedVars n) 
      `isSublistOf` (nodeDependencies n) 
      
    , prop "All local variables are declared" $
      nodeRhsLVars n `isSubsetOf` nodeLhsLVars n
      
    , prop "The imported variables declarations are coherent" $
      let isImported (LVarDescr _ Imported) = True
          isImported _ = False
          imported' = Map.keys $ Map.filter isImported (nodeVars n)
      in (Bimap.keys $ nodeImportedVars n)
         `nubEq` imported' 
    ]
    
--------------------------------------------------------------------------------

specNodesIds :: Spec -> Set NodeId
specNodesIds s = Set.fromList . map nodeId $ specNodes s

--------------------------------------------------------------------------------

instance HasInvariants Spec where
  
  invariants s = 
    [ prop "All mentioned nodes are declared" $
      specTopNodeId s `member` specNodesIds s
      && Set.fromList [nId | n <- specNodes s, nId <- nodeDependencies n] 
         `isSubsetOf` specNodesIds s
         
    , prop "The global vars references are not broken" $
      mconcat (map nodeImportedGVars $ specNodes s) `isSubsetOf`
      mconcat (map nodeExportedGVars $ specNodes s)
        
    , prop "The nodes invariants hold" $ all checkInvs (specNodes s)
    ]

topologicallySorted :: Spec -> Bool
topologicallySorted spec = 
  isJust $ foldM inspect Set.empty (specNodes spec)
  where inspect acc n = do
          guard $ (Set.fromList $ nodeDependencies $ n) `isSubsetOf` acc
          return . Set.insert (nodeId n) $ acc
          
--------------------------------------------------------------------------------
