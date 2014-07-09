--------------------------------------------------------------------------------

module Copilot.Kind.TransSys.Transform
  ( mergeNodes
  , linearize
  , removeCycles ) where

import Copilot.Kind.TransSys.Spec
import Copilot.Kind.TransSys.Renaming
import Copilot.Kind.Misc.Type
import Copilot.Kind.Misc.Operators

import Copilot.Kind.Misc.Utils

import Copilot.Kind.TransSys.Translate (ncSep)

import qualified Data.Map   as Map
import qualified Data.Graph as Graph
import qualified Data.Bimap as Bimap


--------------------------------------------------------------------------------

prefix :: String -> LVar -> LVar
prefix s1 (LVar s2) = LVar $ s1 ++ ncSep ++ s2

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

updateOtherNode :: NodeId -> [NodeId] -> (GVar -> LVar) -> Node -> Node
updateOtherNode newNodeId mergedNodesIds newNameF n = n
  { nodeDependencies = 
      let ds  = nodeDependencies n
          ds' = ds \\ mergedNodesIds
      in if length ds' < length ds then newNodeId : ds' else ds
      
  , nodeImportedVars =
      Bimap.fromList [ (lv, gRename gv) 
                     | (lv, gv) <- Bimap.toList $ nodeImportedVars n ]
  }
  where gRename (gv@(GVar nId _))
          | nId `elem` mergedNodesIds = GVar newNodeId (newNameF gv)
          | otherwise = gv
  
                
mergeNodes :: [NodeId] -> [Node] -> [Node]
mergeNodes toMergeIds nodes =
  newNode : map (updateOtherNode newNodeId toMergeIds updateVar) otherNodes
  
  where
    (toMerge, otherNodes) = partition ((`elem` toMergeIds) . nodeId) nodes
    
    newNodeId = intercalate "-" toMergeIds
    newNode = Node 
      { nodeId = newNodeId
      , nodeDependencies = dependencies
      , nodeImportedVars = importedVars
      , nodeVars = varsDescrs }
       
    -- Computing the dependencies of the new node
    dependencies = nub' 
      [ id | 
        n <- toMerge
      , id <- nodeDependencies n
      , not $ id `elem` toMergeIds ]
      
      
    -- Converting the variables descriptors
    
    updateExpr :: forall t. NodeId -> Expr t -> Expr t
    updateExpr nId (VarE t v) = VarE t (updateVar (GVar nId v))
    updateExpr _ e = e
    
    varsDescrs = Map.fromList $ do
      n <- toMerge
      let nId = nodeId n
      (v, d@(LVarDescr t def)) <- Map.toList $ nodeVars n
      let d' = case def of
           Imported ->
             let ivar = fromJust $ Bimap.lookup v (nodeImportedVars n) in
             if varNode ivar `elem` toMergeIds
             then LVarDescr t $ Expr $ VarE t (updateVar ivar)
             else d
             
           Pre val v' -> LVarDescr t $ Pre val $ updateVar (GVar nId v')
           
           Expr e -> LVarDescr t $ Expr $ transformExpr (updateExpr nId) e
      
      return (updateVar $ GVar nId v, d')
        
  
    -- All the following work is done in the 'Renaming' monad
    (importedVars, updateVar) = runRenaming $ do
      
      -- First the variables which are not imported are renamed
      let isImported (LVarDescr _ Imported) = True
          isImported _                      = False
          
          niVars = [ (nodeId n, v) 
                   | n <- toMerge, (v, d) <- Map.toList (nodeVars n)
                   , not (isImported d) ]
                   
      forM_ niVars $ \(n, v) -> do 
        v' <- getFreshName [n `prefix` v]
        rename n v v'
        
      -- Then 
      let otherNodesMap  = Map.fromList [(nodeId n, n) | n <- otherNodes]
          depsVars = [ (nId, v) 
                     | nId <- dependencies, let n = otherNodesMap ! nId
                     , v <- Map.keys (nodeVars n)]
        
          -- This function 
          checkImport acc (nId, v) = do
            v' <- getFreshName [nId `prefix` v]
            bmap <- forM toMerge $ \n' -> 
                      case Bimap.lookupR (GVar nId v) 
                           (nodeImportedVars n') of
                         
                         Just lv -> rename (nodeId n') lv v' >> return True
                         Nothing -> return False
            
            if any (==True) bmap
              then return $ Bimap.insert v' (GVar nId v) acc
              else return $ acc
      
      foldM checkImport Bimap.empty depsVars
    
    

--------------------------------------------------------------------------------
--
--
--renameLVars :: (Renaming LVar) -> Node -> Node
--renameLVars rename node =
--  node { nodeVars         = nodeVars'
--       , nodeImportedVars = nodeImportedVars' }
--  where
--    
--    nodeVars' = Map.fromList $
--      [ (rename v, updateDescr descr)
--      | (v, descr) <- Map.toList (nodeVars node) ]
--                
--    nodeImportedVars' = Bimap.fromList $
--      [ (rename lvar, gvar)
--      | (lvar, gvar) <- Bimap.toList (nodeImportedVars node) ]
--
--    updateDescr (LVarDescr t def) = LVarDescr t $ case def of
--      Imported    -> Imported
--      Pre val var -> Pre val (rename var)
--      Expr e      -> Expr $ rename updateExpr e
--
--    updateExpr :: forall t . Expr t -> Expr t
--    updateExpr (VarE t var) = VarE t (rename var)
--    updateExpr e = e
--      
--
--updateDescrs :: (LVar -> LVarDescr -> LVarDescr) -> Node -> Node
--updateDescrs update node = node { nodeVars = nodeVars' }
--  where nodeVars' = Map.map update (nodeVars node)
--
-- 
--elimGVars :: [NodeId] -> Node -> Node
--elimGVars mergedIds node = updateDescrs update node
--  where update v d
--          | LVarDescr t Imported <- d,
--            GVar n lvar <- (nodeImportedVars n) ! v,
--            n `elem` mergedIds =
--              LVarDescr t $ Expr $ VarE t $ n `prefix` lvar
--          | otherwise = d
--
--
--renameGVars :: (GVar -> GVar) -> Node -> Node
--renameGVars rename node = updateDescrs update node
--  where update (LVarDescr t (Ext gvar)) = LVarDescr t $ Ext $ rename gvar
--        update d = d
--
--
--
---- | Merge two nodes with the hypothesis that they don't share any
---- local variables names
--unsafeMerge :: NodeId -> [Node] -> Node
--unsafeMerge id nodes =
--  Node { nodeId = id
--       , nodeDependencies = nodeDependencies'
--       , nodeVars = nodeVars'
--       , nodeImportedVars = nodeImportedVars' }
--  where
--    nodeDependencies' = nub' [id | n <- nodes
--                                 , id <- nodeDependencies n
--                                 , not $ id `elem` (map nodeId nodes)]
--                        
--    nodeVars' = foldl Map.union Map.empty (map nodeVars nodes)
--    nodeImportedVars' = undefined
--    
--    
----------------------------------------------------------------------------------
--
--mergeNodes ::  [NodeId] -> [Node] -> [Node]
--mergeNodes toMergeIds nodes =
--  newNode : map (updateDeps . renameGVars renameG) otherNodes
--  
--  where decorateLVars n = renameLVars (prefix (nodeId n)) n
--        
--        (toMerge, otherNodes) = partition ((`elem` toMergeIds) . nodeId) nodes
--
--        newNodeId = intercalate "_" toMergeIds
--        
--        renameG (GVar n lvar)
--          | n `elem` toMergeIds = GVar newNodeId (prefix n lvar)
--          | otherwise = GVar n lvar
--
--        updateDeps n = n { nodeDependencies = update $ nodeDependencies n }
--          where update ds =
--                  let ds' = ds \\ toMergeIds in
--                  if length ds' < length ds then newNodeId : ds' else ds
--        
--        newNode = elimGVars toMergeIds $
--                  unsafeMerge newNodeId (map decorateLVars toMerge)


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
--complete :: Spec -> Spec
--complete spec = spec { specNodes = specNodes' }
--   
--  where
--
--    specNodes' = reverse . foldl completeNode [] . specNodes $ spec
--    
--    -- Takes a list of nodes 'ns', 'n' whose dependencies are in 'ns', and
--    -- returns 'n2:ns' where 'n2' is 'n' completed
--    completeNode :: [Node] -> Node -> [Node]
--    completeNode ns n = (n { nodeVars = nodeVars' }) : ns
--
--      where addedVars = do
--              depId <- nodeDependencies n
--              let Just dep = find ((== depId) . nodeId) ns
--              (lvar, LVarDescr t _) <- Map.toList (nodeVars dep)
--              let gvar = GVar depId lvar              
--              guard . not $ gvar `elem` curExtVars
--              let alias = findName depId lvar
--              return (alias, LVarDescr t $ Ext gvar)
--
--            curExtVars = do
--              (_, LVarDescr _ (Ext gvar)) <- Map.toList (nodeVars n)
--              return gvar
--
--            nodeVars' = Map.union (nodeVars n) (Map.fromList addedVars)
--            
--            findName nId = (nId `prefix`)
--            -- findName nId = head . dropWhile (`isVarOf` n) . iterate (nId `prefix`)
              

--------------------------------------------------------------------------------

isVarOf :: LVar -> Node -> Bool
isVarOf v n = v `elem` (Map.keys $ nodeVars n)

--------------------------------------------------------------------------------
--
---- A list of pairs '(nodeId, aliases)' where : 
---- 'aliases' is a list of (lvar, alias) where 'alias' is the local
---- alias of the variable 'GVar nodeId lvar'
---- sorted alphabetically by the name of the variable being matched
--
--type NodeExtDico  =  Map NodeId [(LVar, LVar)]
--type ExtSpec      =  (Spec,  Map NodeId NodeExtDico)
--
--type Renaming     =  [(GVar, GVar)]
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
--    extVars :: [(NodeId, (LVar, LVar))]
--    extVars =  do
--      (alias, LVarDescr _ (Ext (GVar node var))) <- Map.toList (nodeVars n)
--      return (node, (var, alias))
--  
--
--rmNodeDuplicateImports :: Node -> Node
--rmNodeDuplicateImports node = Map.foldr processDep node (computeExtDico node)
--  where 
--    processDep :: [(LVar, LVar)] -> Node -> Node
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




























