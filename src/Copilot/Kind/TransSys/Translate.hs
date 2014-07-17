--------------------------------------------------------------------------------

module Copilot.Kind.TransSys.Translate ( translate, ncSep ) where

import Copilot.Kind.TransSys.Spec
import Copilot.Kind.Misc.Cast
import Copilot.Kind.Misc.Type
import Copilot.Kind.CoreUtils.Operators


import Copilot.Kind.Misc.Utils
import Control.Monad.State.Lazy

import qualified Copilot.Core as C
import qualified Data.Map     as Map
import qualified Data.Bimap   as Bimap

--------------------------------------------------------------------------------

ncSep         = "."
ncMain        = "out"
ncNode i      = "s" ++ show i
ncPropNode s  = "prop_" ++ s
ncTopNode     = "top"
ncAnonInput   = "in"

ncExternVarNode name = "ext_" ++ name
ncExternFunNode name = "fun_" ++ name
ncExternArrNode name = "arr_" ++ name

ncImported :: NodeId -> String -> String
ncImported n s = n ++ ncSep ++ s

ncTimeAnnot :: String -> Int -> String
ncTimeAnnot s d
  | d == 0    = s
  | otherwise = s ++ ncSep ++ show d

--------------------------------------------------------------------------------

translate :: C.Spec -> Spec
translate cspec =
  Spec { specNodes = [topNode] ++ modelNodes ++ propNodes
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
    
    modelNodes = map stream $ C.specStreams cspec
    propNodes  = mkPropNodes cprops
    topNode    = mkTopNode
                 topNodeId (map nodeId propNodes) cprops

   
--------------------------------------------------------------------------------


mkTopNode :: String -> [NodeId] -> [C.Property] -> Node
mkTopNode topNodeId dependencies cprops = 
  Node { nodeId = topNodeId
       , nodeDependencies = dependencies
       , nodeLocalVars = Map.empty
       , nodeImportedVars = importedVars }
  where
    importedVars = Bimap.fromList 
      [ (Var cp, mkExtVar (ncPropNode cp) ncMain) 
      | cp <- C.propertyName <$> cprops ]
    

mkPropNodes :: [C.Property] -> [Node]
mkPropNodes cprops = map propNode cprops
  where
    propNode p =
      (stream $ streamOfProp p) {nodeId = ncPropNode (C.propertyName p)}


-- A dummy ID is given to this stream, which is not a problem
-- because this ID will never be used
streamOfProp :: C.Property -> C.Stream
streamOfProp prop =
  C.Stream { C.streamId = 42
           , C.streamBuffer = []
           , C.streamExpr = C.propertyExpr prop
           , C.streamExprType = C.Bool }

--------------------------------------------------------------------------------

stream :: C.Stream -> Node
stream (C.Stream { C.streamId
                 , C.streamBuffer
                 , C.streamExpr
                 , C.streamExprType })

  = casting streamExprType $ \t ->
  
    let nodeId = ncNode streamId
        outvar i = Var (ncMain `ncTimeAnnot` i)
    
        (e, nodeDependencies, nodeAuxVars, nodeImportedVars) = 
           runExprTrans t nodeId streamExpr
        buf = map (cast t . toDyn streamExprType) streamBuffer
        
        outputLocals =
          let from i [] = Map.singleton (outvar i) (LVarDescr t $ Expr e)
              from i (b : bs) = 
                 Map.insert (outvar i)
                 (LVarDescr t $ Pre b $ outvar (i + 1))
                 $ from (i + 1) bs
          in from 0 buf
        
        nodeLocalVars = Map.union nodeAuxVars outputLocals
        nodeOutputs = map outvar [0 .. length buf - 1]
        
    in Node { nodeId, nodeDependencies, nodeLocalVars, nodeImportedVars }

--------------------------------------------------------------------------------

expr :: forall t t' . Type t -> C.Expr t' -> Trans (Expr t)

expr t (C.Const t' v) = return $ Const t (cast t $ toDyn t' v)

expr t (C.Drop _ (fromIntegral -> k :: Int) id) = do
  let node = ncNode id
  selfRef <- (== node) <$> curNode
  let varName = ncMain `ncTimeAnnot` k
  let var = Var $ if selfRef then varName else ncImported node varName
  when (not selfRef) $ do
    newDep node
    newImportedVar var (mkExtVar node varName)
  return $ VarE t var


expr t (C.Local tl _tr id l e) = casting tl $ \tl' -> do
  l' <- expr tl' l
  newLocal (Var id) $ LVarDescr tl' $ Expr l'
  expr t e

expr t (C.Var _t' id) = return $ VarE t (Var id)

expr t (C.Op3 (C.Mux _) cond e1 e2) = do
  cond' <- expr Bool cond
  e1'   <- expr t    e1
  e2'   <- expr t    e2
  return $ Ite t cond' e1' e2'
  
expr t (C.ExternVar _ name _) = do
  let nodeName = ncExternVarNode name
  let localAlias = Var ("extvar_" ++ name)
  newDep nodeName
  newImportedVar localAlias (ExtVar nodeName (Var ncMain))
  return $ VarE t localAlias
  
expr t (C.Op1 op e) = handleOp1
  t (op, e) expr notHandled Op1
  
  where notHandled (UnhandledOp1 opName ta tb) = do
          error "not handled"
          -- e' <- U <$> expr ta e
          --newAnonFun $ AnonFunDescr (ncUnhandledOp opName) tb [U ta]
          --return $ FunApp t (ncUnhandledOp opName) [e']

expr t (C.Op2 op e1 e2) = handleOp2
  t (op, e1, e2) expr notHandled Op2 (Op1 Bool Not)
  
  where notHandled (UnhandledOp2 opName ta tb tc) = do
          error "not handled"
--          e1' <- U <$> expr ta e1
--          e2' <- U <$> expr tb e2
--          newAnonFun $ AnonFunDescr (ncUnhandledOp opName) tc [U ta, U tb]
--          return $ FunApp t (ncUnhandledOp opName) [e1', e2']
  
expr t (C.ExternFun ta name args _ mtag) = do
  let inputVars = mapM makeArgLocalAlias (zip [0..] args)
  error "Not handled"
  
  where 
    nodeName = ncExternFunNode name
  
    makeArgLocalAlias :: (Int, C.UExpr) -> Trans Var
    makeArgLocalAlias (argN, C.UExpr {C.uExprExpr, C.uExprType}) =
      casting uExprType $ \ta -> do
        let argvar = Var $ nodeName ++ "_arg_" ++ show argN
        e <- expr ta uExprExpr
        newLocal argvar (LVarDescr ta $ Expr e)
        return argvar
          
          

--expr t (C.Drop _ (fromIntegral -> k :: Int) id) = do
--  let node = ncNode id
--  selfRef <- (== node) <$> curNode
--  let varName = ncMain `ncTimeAnnot` k
--  let var = Var $ if selfRef then varName else ncImported node varName
--  when (not selfRef) $ do
--    newDep node
--    newImportedVar var (mkExtVar node varName)
--  return $ VarE t var


-- ExternVar    :: Type a -> Name -> Maybe [a] -> Expr a 
--  ExternFun    :: Type a -> Name -> [UExpr] -> Maybe (Expr a) 
--               -> Maybe Tag -> Expr a
--  ExternArray  :: Integral a => Type a -> Type b -> Name -> Int -> Expr a
--               -> Maybe [[b]] -> Maybe Tag -> Expr b 
  
--expr _ _ = error "This kind of expression is not handled yet"

--------------------------------------------------------------------------------

-- | Parses the expression
-- Returns : (expr, new dependencies, 
-- new local variables, new imported variables)
-- There are lots of boilerplate here. Maybe we should use 'lens'

runExprTrans :: Type t -> NodeId -> C.Expr a -> 
               (Expr t, [NodeId], Map Var LVarDescr, Bimap Var ExtVar )
               
runExprTrans t curNode e = 
  (e', nub' (_dependencies s), _lvars s, _importedVars s)
  where (e', s) = runState (expr t e) (TransSt Map.empty Bimap.empty [] curNode)


data TransSt = TransSt 
  { _lvars        :: Map Var LVarDescr
  , _importedVars :: Bimap Var ExtVar
  , _dependencies :: [NodeId]
  , _curNode      :: NodeId }
               
type Trans a = State TransSt a

newDep :: NodeId -> Trans ()
newDep d =  modify $ \s -> s { _dependencies = d : _dependencies s }

newImportedVar :: Var -> ExtVar -> Trans ()
newImportedVar l g = modify $ 
  \s -> s { _importedVars = Bimap.insert l g (_importedVars s) }

newLocal :: Var -> LVarDescr -> Trans ()
newLocal l d  =  modify $ \s -> s { _lvars = Map.insert l d $ _lvars s }

curNode :: Trans NodeId
curNode =  _curNode <$> get

--------------------------------------------------------------------------------
