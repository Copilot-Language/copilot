--------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes, NamedFieldPuns, ViewPatterns,
             ScopedTypeVariables, GADTs, FlexibleContexts #-}
{-# LANGUAGE Safe #-}

module Copilot.Theorem.TransSys.Translate ( translate ) where

import Copilot.Theorem.TransSys.Spec
import Copilot.Theorem.TransSys.Cast
import Copilot.Theorem.Misc.Utils

import Control.Monad.State.Lazy

import Data.Char (isNumber)
import Data.Function (on)

import Data.Map (Map)
import Data.Bimap (Bimap)

import qualified Copilot.Core as C
import qualified Data.Map     as Map
import qualified Data.Bimap   as Bimap

--------------------------------------------------------------------------------

-- Naming conventions
-- These are important in order to avoid name conflicts

ncSep         = "."
ncMain        = "out"
ncNode i      = "s" ++ show i
ncPropNode s  = "prop-" ++ s
ncTopNode     = "top"
ncAnonInput   = "in"
ncLocal s     = "l" ++ dropWhile (not . isNumber) s

ncExternVarNode name = "ext-" ++ name

ncImported :: NodeId -> String -> String
ncImported n s = n ++ ncSep ++ s

ncTimeAnnot :: String -> Int -> String
ncTimeAnnot s d
  | d == 0    = s
  | otherwise = s ++ ncSep ++ show d

--------------------------------------------------------------------------------

translate :: C.Spec -> TransSys
translate cspec =

  TransSys { specNodes = [topNode] ++ modelNodes ++ propNodes ++ extVarNodes
       , specTopNodeId = topNodeId
       , specProps = propBindings }

  where

    topNodeId = ncTopNode

    cprops :: [C.Property]
    cprops = C.specProperties cspec

    propBindings :: Map PropId ExtVar
    propBindings = Map.fromList $ do
      pid <- map C.propertyName cprops
      return (pid, mkExtVar topNodeId pid)

    ((modelNodes, propNodes), extvarNodesNames) = runTrans $
      liftM2 (,) (mapM stream (C.specStreams cspec)) (mkPropNodes cprops)

    topNode = mkTopNode topNodeId (map nodeId propNodes) cprops
    extVarNodes = map mkExtVarNode extvarNodesNames

--------------------------------------------------------------------------------


mkTopNode :: String -> [NodeId] -> [C.Property] -> Node
mkTopNode topNodeId dependencies cprops =
  Node { nodeId = topNodeId
       , nodeDependencies = dependencies
       , nodeLocalVars = Map.empty
       , nodeImportedVars = importedVars
       , nodeConstrs = []}
  where
    importedVars = Bimap.fromList
      [ (Var cp, mkExtVar (ncPropNode cp) ncMain)
      | cp <- C.propertyName <$> cprops ]



mkExtVarNode (name, U t) =
  Node { nodeId = name
       , nodeDependencies = []
       , nodeLocalVars = Map.singleton (Var ncMain) (VarDescr t $ Constrs [])
       , nodeImportedVars = Bimap.empty
       , nodeConstrs = []}


mkPropNodes :: [C.Property] -> Trans [Node]
mkPropNodes = mapM propNode
  where
    propNode p = do
      s <- stream (streamOfProp p)
      return $ s {nodeId = ncPropNode (C.propertyName p)}

-- A dummy ID is given to this stream, which is not a problem
-- because this ID will never be used
streamOfProp :: C.Property -> C.Stream
streamOfProp prop =
  C.Stream { C.streamId = 42
           , C.streamBuffer = []
           , C.streamExpr = C.propertyExpr prop
           , C.streamExprType = C.Bool }

--------------------------------------------------------------------------------

stream :: C.Stream -> Trans Node
stream (C.Stream { C.streamId
                 , C.streamBuffer
                 , C.streamExpr
                 , C.streamExprType })

  = casting streamExprType $ \t -> do

    let nodeId = ncNode streamId
        outvar i = Var (ncMain `ncTimeAnnot` i)
        buf = map (cast t . toDyn streamExprType) streamBuffer

    (e, nodeAuxVars, nodeImportedVars, nodeDependencies) <-
      runExprTrans t nodeId streamExpr

    let outputLocals =
          let from i [] = Map.singleton (outvar i) (VarDescr t $ Expr e)
              from i (b : bs) =
                 Map.insert (outvar i)
                 (VarDescr t $ Pre b $ outvar (i + 1))
                 $ from (i + 1) bs
          in from 0 buf
        nodeLocalVars = Map.union nodeAuxVars outputLocals
        nodeOutputs = map outvar [0 .. length buf - 1]

    return Node
      { nodeId, nodeDependencies, nodeLocalVars
      , nodeImportedVars, nodeConstrs = [] }

--------------------------------------------------------------------------------

expr :: Type t -> C.Expr t' -> Trans (Expr t)

expr t (C.Const t' v) = return $ Const t (cast t $ toDyn t' v)

expr t (C.Drop _ (fromIntegral -> k :: Int) id) = do
  let node = ncNode id
  selfRef <- (== node) <$> curNode
  let varName = ncMain `ncTimeAnnot` k
  let var = Var $ if selfRef then varName else ncImported node varName
  unless selfRef $ do
    newDep node
    newImportedVar var (mkExtVar node varName)
  return $ VarE t var

expr t (C.Label _ _ e) = expr t e

expr t (C.Local tl _tr id l e) = casting tl $ \tl' -> do
  l' <- expr tl' l
  newLocal (Var $ ncLocal id) $ VarDescr tl' $ Expr l'
  expr t e

expr t (C.Var _t' id) = return $ VarE t (Var $ ncLocal id)

expr t (C.Op3 (C.Mux _) cond e1 e2) = do
  cond' <- expr Bool cond
  e1'   <- expr t    e1
  e2'   <- expr t    e2
  return $ Ite t cond' e1' e2'

expr t (C.ExternVar _ name _) = do
  let nodeName = ncExternVarNode name
  let localAlias = Var nodeName
  newExtVarNode nodeName (U t)
  newDep nodeName
  newImportedVar localAlias (ExtVar nodeName (Var ncMain))
  return $ VarE t localAlias

-- TODO : Use uninterpreted functions to handle
-- * Unhandled operators
-- * Extern functions
-- * Extern arrays
-- For now, the result of these operations is a new unconstrained variable

expr t (C.Op1 op e) = handleOp1
  t (op, e) expr notHandled Op1
  where
    notHandled (UnhandledOp1 _opName _ta _tb) =
      newUnconstrainedVar t

expr t (C.Op2 op e1 e2) = handleOp2
  t (op, e1, e2) expr notHandled Op2 (Op1 Bool Not)
  where
    notHandled (UnhandledOp2 _opName _ta _tb _tc) =
      newUnconstrainedVar t

newUnconstrainedVar :: Type t -> Trans (Expr t)
newUnconstrainedVar t = do
  newNode <- getFreshNodeName
  newLocal (Var newNode) $ VarDescr t $ Constrs []
  newDep newNode
  return $ VarE t (Var newNode)

--------------------------------------------------------------------------------

runTrans :: Trans a -> (a, [(NodeId, U Type)])
runTrans mx =
  (x, nubBy' (compare `on` fst) $ _extVarsNodes st)
  where
    (x, st) = runState mx initState
    initState = TransSt
      { _lvars        = Map.empty
      , _importedVars = Bimap.empty
      , _dependencies = []
      , _extVarsNodes = []
      , _curNode      = ""
      , _nextUid      = 0 }

runExprTrans ::
  Type t -> NodeId -> C.Expr a ->
  Trans (Expr t, Map Var VarDescr, Bimap Var ExtVar, [NodeId])

runExprTrans t curNode e = do
  modify $ \st -> st { _curNode = curNode }
  modify $ \st -> st { _nextUid = 0 }
  e' <- expr t e
  (lvs, ivs, dps) <- popLocalInfos
  return (e', lvs, ivs, dps)

data TransSt = TransSt
  { _lvars        :: Map Var VarDescr
  , _importedVars :: Bimap Var ExtVar
  , _dependencies :: [NodeId]
  , _extVarsNodes :: [(NodeId, U Type)]
  , _curNode      :: NodeId
  , _nextUid      :: Int }

type Trans a = State TransSt a

newDep d =  modify $ \s -> s { _dependencies = d : _dependencies s }

popLocalInfos :: State TransSt (Map Var VarDescr, Bimap Var ExtVar, [NodeId])
popLocalInfos = do
  lvs <- _lvars <$> get
  ivs <- _importedVars <$> get
  dps <- _dependencies <$> get
  modify $ \st -> st
    { _lvars = Map.empty
    , _importedVars = Bimap.empty
    , _dependencies = [] }
  return (lvs, ivs, nub' dps)


getUid :: Trans Int
getUid = do
  uid <- _nextUid <$> get
  modify $ \st -> st { _nextUid = uid + 1 }
  return uid

getFreshNodeName :: Trans NodeId
getFreshNodeName = liftM (("_" ++) . show) getUid

newImportedVar l g = modify $
  \s -> s { _importedVars = Bimap.insert l g (_importedVars s) }

newLocal l d  =  modify $ \s -> s { _lvars = Map.insert l d $ _lvars s }

curNode = _curNode <$> get

newExtVarNode id t =
  modify $ \st -> st { _extVarsNodes = (id, t) : _extVarsNodes st }

--------------------------------------------------------------------------------
