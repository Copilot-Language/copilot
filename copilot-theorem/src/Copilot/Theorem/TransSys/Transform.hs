{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe       #-}

-- | Helper module to manipulate and simplify TransSys graphs.
module Copilot.Theorem.TransSys.Transform
  ( mergeNodes
  , inline
  , removeCycles
  , complete
  ) where

import Copilot.Theorem.TransSys.Spec
import Copilot.Theorem.TransSys.Renaming

import Copilot.Theorem.Misc.Utils

import Control.Monad (foldM, forM_, forM, guard)

import Data.List (sort, (\\), intercalate, partition)

import Control.Exception.Base (assert)

import Data.Map (Map, (!))
import Data.Set (member)
import Data.Bimap (Bimap)

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Graph as Graph
import qualified Data.Bimap as Bimap

prefix :: String -> Var -> Var
prefix s1 (Var s2) = Var $ s1 ++ "." ++ s2

ncNodeIdSep = "-"

-- | Merge all the given nodes, replacing all references to the given node Ids
-- with a reference to a fresh node id (unless the nodes given as argument
-- contain the top node), in which case its ID is chosen instead.
mergeNodes :: [NodeId] -> TransSys -> TransSys
mergeNodes toMergeIds spec =
  spec
    { specNodes = newNode :
        map (updateOtherNode newNodeId toMergeIds renamingExtF) otherNodes
    , specProps =
        Map.map (\(ev, prop) -> (renamingExtF ev, prop)) (specProps spec) }

  where
    nodes = specNodes spec
    (toMerge, otherNodes) = partition ((`elem` toMergeIds) . nodeId) nodes

    -- Choosing the new node ID. If the top node is merged,
    -- its name is kept
    newNodeId
      | specTopNodeId spec `elem` toMergeIds = specTopNodeId spec
      | otherwise = intercalate ncNodeIdSep (sort toMergeIds)

    newNode = Node
      { nodeId = newNodeId
      , nodeDependencies = dependencies
      , nodeImportedVars = importedVars
      , nodeLocalVars = localVars
      , nodeConstrs = constrs }

    -- Computing the dependencies of the new node
    dependencies = nub'
      [ id |
        n <- toMerge
      , id <- nodeDependencies n
      , id `notElem` toMergeIds ]

    -- All the work of renaming is done in the 'Misc.Renaming' monad. Some code
    -- complexity has been added so the variable names remains as clear as
    -- possible after merging two nodes.
    (importedVars, renamingF) = runRenaming $ do
      renameLocalVars toMerge
      redirectLocalImports toMerge
      selectImportedVars toMerge otherNodes dependencies

    -- Converting the variables descriptors
    localVars = mergeVarsDescrs toMerge renamingF

    -- Computing the global renaming function
    renamingExtF (gv@(ExtVar nId _))
     | nId `elem` toMergeIds = ExtVar newNodeId (renamingF gv)
     | otherwise = gv

    constrs = mergeConstrs toMerge renamingF

updateOtherNode :: NodeId -> [NodeId] -> (ExtVar -> ExtVar) -> Node -> Node
updateOtherNode newNodeId mergedNodesIds renamingF n = n
  { nodeDependencies =
      let ds  = nodeDependencies n
          ds' = ds \\ mergedNodesIds
      in if length ds' < length ds then newNodeId : ds' else ds

  , nodeImportedVars =
      Bimap.fromList [ (lv, renamingF gv)
                     | (lv, gv) <- Bimap.toList $ nodeImportedVars n ]
  }

updateExpr :: NodeId -> (ExtVar -> Var) -> Expr t -> Expr t
updateExpr nId renamingF = transformExpr aux
  where
    aux :: forall t. Expr t -> Expr t
    aux (VarE t v) = VarE t (renamingF (ExtVar nId v))
    aux e = e

mergeVarsDescrs :: [Node] -> (ExtVar -> Var) -> Map Var VarDescr
mergeVarsDescrs toMerge renamingF = Map.fromList $ do
  n <- toMerge
  let nId = nodeId n
  (v, VarDescr t def) <- Map.toList $ nodeLocalVars n
  let d' = case def of
       Pre val v' -> VarDescr t $
         Pre val $ renamingF (ExtVar nId v')
       Expr e -> VarDescr t $
         Expr $ updateExpr nId renamingF e
       Constrs cs -> VarDescr t $
         Constrs $ map (updateExpr nId renamingF) cs

  return (renamingF $ ExtVar nId v, d')

mergeConstrs :: [Node] -> (ExtVar -> Var) -> [Expr Bool]
mergeConstrs toMerge renamingF =
  [ updateExpr (nodeId n) renamingF c | n <- toMerge, c <- nodeConstrs n ]

renameLocalVars :: [Node] -> Renaming ()
renameLocalVars toMerge =
  forM_ niVars $ \(n, v) -> do
    v' <- getFreshName [n `prefix` v]
    rename n v v'
  where
  niVars = [ (nodeId n, v)
           | n <- toMerge, (v, _) <- Map.toList (nodeLocalVars n) ]

selectImportedVars :: [Node] -> [Node] -> [NodeId]
                      -> Renaming (Bimap Var ExtVar)
selectImportedVars toMerge otherNodes dependencies =
  foldM checkImport Bimap.empty depsVars

  where
    otherNodesMap = Map.fromList [(nodeId n, n) | n <- otherNodes]

    depsVars = [ (nId, v)
                 | nId <- dependencies, let n = otherNodesMap ! nId
                 , v <- Map.keys (nodeLocalVars n)]

    checkImport acc (nId, v) = do
        v' <- getFreshName [nId `prefix` v]
        bmap <- forM toMerge $ \n' ->
                  case Bimap.lookupR (ExtVar nId v)
                       (nodeImportedVars n') of

                     Just lv -> rename (nodeId n') lv v' >> return True
                     Nothing -> return False

        return $
          if True `elem` bmap
            then Bimap.insert v' (ExtVar nId v) acc
            else acc

redirectLocalImports :: [Node] -> Renaming ()
redirectLocalImports toMerge = do
  renamingF <- getRenamingF
  forM_ x $ \(n, alias, n', v) ->
    rename n alias (renamingF (ExtVar n' v))

  where
    mergedNodesSet = Set.fromList [nodeId n | n <- toMerge]
    x = do
      n <- toMerge
      let nId = nodeId n
      (alias, ExtVar n' v) <- Bimap.toList (nodeImportedVars n)
      guard $ n' `member` mergedNodesSet
      return (nId, alias, n', v)

-- | Discard all the structure of a /modular transition system/ and turn it
-- into a /non-modular transition system/ with only one node.
inline :: TransSys -> TransSys
inline spec = mergeNodes [nodeId n | n <- specNodes spec] spec

-- | Remove cycles by merging nodes participating in strongly connected
-- components.
--
-- The transition system obtained by the 'TransSys.Translate' module is
-- perfectly consistent. However, it can't be directly translated into the
-- /Kind2 native file format/. Indeed, it is natural to bind each node to a
-- predicate but the Kind2 file format requires that each predicate only uses
-- previously defined predicates. However, some nodes in our transition system
-- could be mutually recursive. Therefore, the goal of 'removeCycles' is to
-- remove such dependency cycles.
--
-- The function 'removeCycles' computes the strongly connected components of
-- the dependency graph and merge each one into a single node using
-- 'mergeNodes'. The complexity of this process is high in the worst case (the
-- square of the total size of the system times the size of the biggest node)
-- but good in practice as few nodes are to be merged in most practical cases.
removeCycles :: TransSys -> TransSys
removeCycles spec =
  topoSort $ foldr mergeComp spec (buildScc nodeId $ specNodes spec)
  where

    mergeComp (Graph.AcyclicSCC _)  s = s
    mergeComp (Graph.CyclicSCC ids) s = mergeNodes ids s

    buildScc nrep ns =
     let depGraph = map (\n -> (nrep n, nodeId n, nodeDependencies n)) ns
     in Graph.stronglyConnComp depGraph

    topoSort s = s { specNodes =
      map (\(Graph.AcyclicSCC n) -> n) $ buildScc id (specNodes s) }

-- | Completes each node of a specification with imported variables such that
-- each node contains a copy of all its dependencies.
--
-- The given specification should have its node sorted by topological order.
--
-- The top nodes should have all the other nodes as its dependencies.

complete :: TransSys -> TransSys
complete spec =
  assert (isTopologicallySorted spec) $ spec { specNodes = specNodes' }

  where

    specNodes' =
      reverse
      . foldl completeNode []
      . specNodes
      . completeTopNodeDeps
      $ spec

    completeTopNodeDeps spec = spec { specNodes = map aux nodes }
      where
        nodes = specNodes spec
        aux n
          | nodeId n == specTopNodeId spec =
              n { nodeDependencies = map nodeId nodes \\ [nodeId n] }
          | otherwise = n

    -- Takes a list of nodes 'ns', 'n' whose dependencies are in 'ns', and
    -- returns 'n2:ns' where 'n2' is 'n' completed
    completeNode :: [Node] -> Node -> [Node]
    completeNode ns n = (n { nodeDependencies = dependencies'
                           , nodeImportedVars = importedVars' }) : ns

      where
        nsMap = Map.fromList [(nodeId n, n) | n <- ns]
        dependencies' =
          let newDeps = do
                dId <- nodeDependencies n
                let d = nsMap ! dId
                nodeDependencies d

          in nub' $ nodeDependencies n ++ newDeps

        importedVars' = fst . runRenaming $ do
          forM_ (Set.toList $ nodeVarsSet n) addReservedName
          let toImportVars = nub' [ ExtVar nId v
                                  | nId <- dependencies'
                                  , let n' = nsMap ! nId
                                  , v <- Map.keys (nodeLocalVars n') ]

              tryImport acc ev@(ExtVar n' v) = do

                -- To get readable names, we don't prefix variables
                -- which come from merged nodes as they are already
                -- decorated
                let preferedName
                     | head ncNodeIdSep `elem` n' = v
                     | otherwise = n' `prefix` v
                alias <- getFreshName [preferedName, n' `prefix` v]
                return $ Bimap.tryInsert alias ev acc

          foldM tryImport (nodeImportedVars n) toImportVars
