--------------------------------------------------------------------------------

module Copilot.Kind.TransSys.Transform
  ( mergeNodes
  , linearize
  , removeCycles 
  , complete ) where

import Copilot.Kind.TransSys.Spec
import Copilot.Kind.TransSys.Renaming
import Copilot.Kind.Misc.Type
import Copilot.Kind.Misc.Operators

import Copilot.Kind.Misc.Utils

import Copilot.Kind.TransSys.Translate (ncSep)

import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Graph as Graph
import qualified Data.Bimap as Bimap


--------------------------------------------------------------------------------

prefix :: String -> Var -> Var
prefix s1 (Var s2) = Var $ s1 ++ ncSep ++ s2

ncNodeIdSep = "-"

--------------------------------------------------------------------------------

mergeNodes :: [NodeId] -> [Node] -> [Node]
mergeNodes toMergeIds nodes =
  newNode : map (updateOtherNode newNodeId toMergeIds renamingF) otherNodes
  
  where
    (toMerge, otherNodes) = partition ((`elem` toMergeIds) . nodeId) nodes
    
    newNodeId = intercalate ncNodeIdSep toMergeIds
    newNode = Node 
      { nodeId = newNodeId
      , nodeDependencies = dependencies
      , nodeImportedVars = importedVars
      , nodeLocalVars = localVars }
       
    -- Computing the dependencies of the new node
    dependencies = nub' 
      [ id | 
        n <- toMerge
      , id <- nodeDependencies n
      , not $ id `elem` toMergeIds ]
  
    -- All the work of renaming is done in the monad with the same name
    (importedVars, renamingF) = runRenaming $ do
      renameLocalVars toMerge
      redirectLocalImports toMerge
      selectImportedVars toMerge otherNodes dependencies
      
    -- Converting the variables descriptors
    localVars = mergeVarsDescrs toMerge renamingF




updateOtherNode :: NodeId -> [NodeId] -> (ExtVar -> Var) -> Node -> Node
updateOtherNode newNodeId mergedNodesIds newNameF n = n
  { nodeDependencies = 
      let ds  = nodeDependencies n
          ds' = ds \\ mergedNodesIds
      in if length ds' < length ds then newNodeId : ds' else ds
      
  , nodeImportedVars =
      Bimap.fromList [ (lv, gRename gv) 
                     | (lv, gv) <- Bimap.toList $ nodeImportedVars n ]
  }
  where gRename (gv@(ExtVar nId _))
          | nId `elem` mergedNodesIds = ExtVar newNodeId (newNameF gv)
          | otherwise = gv
          
          
          
          
mergeVarsDescrs :: [Node] -> (ExtVar -> Var) -> Map Var LVarDescr
mergeVarsDescrs toMerge renamingF = Map.fromList $ do
  n <- toMerge
  let nId = nodeId n
  (v, (LVarDescr t def)) <- Map.toList $ nodeLocalVars n
  let d' = case def of
       Pre val v' -> LVarDescr t $ Pre val $ renamingF (ExtVar nId v')
       Expr e -> LVarDescr t $ Expr $ transformExpr (updateExpr nId) e
  
  return (renamingF $ ExtVar nId v, d')

  where 
    toMergeIds = map nodeId toMerge
    
    updateExpr :: forall t. NodeId -> Expr t -> Expr t
    updateExpr nId (VarE t v) = VarE t (renamingF (ExtVar nId v))
    updateExpr _ e = e




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
        
        if any (==True) bmap
          then return $ Bimap.insert v' (ExtVar nId v) acc
          else return $ acc



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

linearize :: Spec -> Spec
linearize spec = spec { specNodes = update (specNodes spec) }
  where update ns = setId "top" <$> mergeNodes (map nodeId ns) ns
        setId id n = n { nodeId = id }

removeCycles :: Spec -> Spec
removeCycles spec = spec { specNodes = update (specNodes spec) }
  where update ns =
          let scc = buildScc nodeId ns
            
              mergeComp (Graph.AcyclicSCC _) ns = ns
              mergeComp (Graph.CyclicSCC ids) ns =
                mergeNodes ids ns
            
          in topoSort $ foldr mergeComp ns scc

        buildScc nrep ns =
          let depGraph = map (\n -> (nrep n, nodeId n, nodeDependencies n)) ns
          in Graph.stronglyConnComp depGraph

        topoSort ns = map (\(Graph.AcyclicSCC n) -> n) $ buildScc id ns
          


-- | Completes each node of a specification with imported variables such
-- | that each node contains a copy of all its dependencies
-- | The given specification should have its node sorted by topological
-- | order.
-- | Note that the last node will contain all the needed variables to
-- | characterize the whole system
--

complete :: Spec -> Spec
complete spec = 
  assert (isTopologicallySorted spec) 
  $ spec { specNodes = specNodes' }
   
  where

    specNodes' = reverse . foldl completeNode [] . specNodes $ spec
    
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
                alias <- getFreshName [preferedName]
                return $ Bimap.tryInsert alias ev acc
                
          
          foldM tryImport (nodeImportedVars n) toImportVars
          
              

--------------------------------------------------------------------------------
--
---- A list of pairs '(nodeId, aliases)' where : 
---- 'aliases' is a list of (lvar, alias) where 'alias' is the local
---- alias of the variable 'ExtVar nodeId lvar'
---- sorted alphabetically by the name of the variable being matched
--
--type NodeExtDico  =  Map NodeId [(Var, Var)]
--type ExtSpec      =  (Spec,  Map NodeId NodeExtDico)
--
--type Renaming     =  [(ExtVar, ExtVar)]
--
--
---- Compute the dictionnary of extern variables
--
--computeExtDico :: Node -> NodeExtDico
--computeExtDico n = 
--  Map.fromList 
--  . map (\l -> (fst . head $ l, map snd l))
--  . groupBy ((==) `on` fst)
--  . sortBy lexicord
--  $ extVars
--  where
--    lexicord x y = (compare `on` fst) x y <> (compare `on` (fst . snd)) x y
--
--    extVars :: [(NodeId, (Var, Var))]
--    extVars =  do
--      (alias, LVarDescr _ (Ext (ExtVar node var))) <- Map.toList (nodeVars n)
--      return (node, (var, alias))
--  
--
--rmNodeDuplicateImports :: Node -> Node
--rmNodeDuplicateImports node = Map.foldr processDep node (computeExtDico node)
--  where 
--    processDep :: [(Var, Var)] -> Node -> Node
--    processDep bindings n =
--      let bs = filter ((>= 2) . length) . groupBy ((==) `on` fst) $ bindings
--          redirect ((_, mainAlias) : (map snd -> duplicates)) nvars = 
--            foldr (\alias -> Map.update (\(LVarDescr t _) ->
--                             Just . LVarDescr t . Expr $ VarE t mainAlias)
--                             alias )
--                   nvars duplicates
--                   
--      in n {nodeVars = foldr redirect (nodeVars n) bs}
--
--
--removeDuplicateImports :: Spec -> Spec
--removeDuplicateImports spec = 
--  spec {specNodes = map rmNodeDuplicateImports (specNodes spec)}
          
--          
--      
--
--prepare :: Spec -> ExtSpec
--prepare spec = foldr processNode (spec, Map.empty) $ map nodeId (specNodes spec)
--  where
--    
--    processNode :: NodeId -> ExtSpec -> ExtSpec
--    processNode nId spec = 
--      
--      let nodes = specNodes spec
--          ([node], otherNodes) = partition ((== nId) . nodeId) nodes
--          extDico = computeExtDico node
--          
--      in undefined
--
--      

--------------------------------------------------------------------------------




























