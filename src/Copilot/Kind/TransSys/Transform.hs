--------------------------------------------------------------------------------

module Copilot.Kind.TransSys.Transform
  ( mergeNodes
  , inline
  , removeCycles 
  , complete 
  ) where

import Copilot.Kind.TransSys.Spec
import Copilot.Kind.TransSys.Renaming
import Copilot.Kind.Misc.Type
import Copilot.Kind.Misc.Operators

import Copilot.Kind.Misc.Utils

import Data.List (sort)

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Graph as Graph
import qualified Data.Bimap as Bimap

import qualified Copilot.Kind.Misc.Error as Err

--------------------------------------------------------------------------------

prefix :: String -> Var -> Var
prefix s1 (Var s2) = Var $ s1 ++ "." ++ s2

ncNodeIdSep = "-"

--------------------------------------------------------------------------------

mergeNodes :: [NodeId] -> Spec -> Spec
mergeNodes toMergeIds spec =
  spec
    { specNodes = newNode : 
        map (updateOtherNode newNodeId toMergeIds renamingExtF) otherNodes
    , specProps = Map.map renamingExtF (specProps spec) }
      
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
  
    -- All the work of renaming is done in the monad with the same name
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


mergeVarsDescrs :: [Node] -> (ExtVar -> Var) -> Map Var LVarDescr
mergeVarsDescrs toMerge renamingF = Map.fromList $ do
  n <- toMerge
  let nId = nodeId n
  (v, LVarDescr t def) <- Map.toList $ nodeLocalVars n
  let d' = case def of
       Pre val v' -> LVarDescr t $ 
         Pre val $ renamingF (ExtVar nId v')
       Expr e -> LVarDescr t $ 
         Expr $ updateExpr nId renamingF e
       Constrs cs -> LVarDescr t $ 
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
      
--------------------------------------------------------------------------------

inline :: Spec -> Spec
inline spec = mergeNodes (map nodeId $ specNodes spec) spec

removeCycles :: Spec -> Spec
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
          
--------------------------------------------------------------------------------

-- | Completes each node of a specification with imported variables such
-- | that each node contains a copy of all its dependencies
-- | The given specification should have its node sorted by topological
-- | order.
-- | The top nodes should have all the other nodes as its dependencies

complete :: Spec -> Spec
complete spec = 
  assert (isTopologicallySorted spec)
  $ spec { specNodes = specNodes' }
   
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
              n { nodeDependencies = (map nodeId nodes) \\ [nodeId n] }
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
                ddId <- nodeDependencies d
                return ddId
          
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
          
--------------------------------------------------------------------------------