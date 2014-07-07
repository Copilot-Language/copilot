--------------------------------------------------------------------------------

module Copilot.Kind.TransSys.Translate ( translate ) where

import Copilot.Kind.TransSys.Spec
import Copilot.Kind.Misc.Casted
import Copilot.Kind.Misc.Type

import Copilot.Kind.Misc.Utils
import Control.Monad.State.Lazy

import qualified Copilot.Core as C
import qualified Data.Map as Map

--------------------------------------------------------------------------------

-- Naming conventions :
-- Each node defines :
-- * Some output variables out, out.1, out.2 ... out.d where d is the depth
--   of the node
-- * For each external node ext which is accessed, ext.out[.i]
-- * For each local variable
-- Prefix 'nc' stands for "naming convention"
-- Unicity of names is guaranted by ...

ncMain = "out"
ncNode i = "s" ++ show i
ncPropNode s = "p" ++ s

ncExt :: NodeId -> String -> String
ncExt n s = n ++ "." ++ s

ncTimeAnnot :: String -> Int -> String
ncTimeAnnot s d
  | d == 0    = s
  | otherwise = s ++ "." ++ show d

--------------------------------------------------------------------------------


translate :: C.Spec -> Spec
translate cspec =
  Spec { specNodes = [topNode] ++ modelNodes ++ propNodes
       , specTopNodeId = topNodeId
       , specProps = propBindings
       , specAssertDeps = assertDeps }

  where

    topNodeId = "top"
    
    cprops :: [C.Property]
    cprops = C.specProperties cspec

    assumptions :: [PropId]
    assumptions = do
      C.Property C.Assumption pid _ <- cprops
      return pid

    assertDeps :: Map PropId [PropId]
    assertDeps = Map.fromList $ do
      C.Property (C.Assertion dps) pid _ <- cprops
      return (pid, dps ++ assumptions)

    propBindings :: Map PropId GVar
    propBindings = Map.fromList $ do
      pid <- map C.propertyName cprops
      return (pid, mkGVar topNodeId pid)
    
    modelNodes = map stream $ C.specStreams cspec
    propNodes  = mkPropNodes cprops
    topNode    = mkTopNode
                 topNodeId (map nodeId propNodes) cprops

   
--------------------------------------------------------------------------------


mkTopNode :: String -> [NodeId] -> [C.Property] -> Node
mkTopNode topNodeId dependencies cprops = 
  Node { nodeId = topNodeId
       , nodeDependencies = dependencies
       , nodeVars = topNodeVars }
  where
    topNodeVars =
      foldr ( \p -> Map.insert (LVar p) $
                    LVarDescr Bool $ Ext $ mkGVar (ncPropNode p) ncMain )
      Map.empty (map C.propertyName $ cprops)
    

mkPropNodes :: [C.Property] -> [Node]
mkPropNodes cprops = map propNode cprops
  where
    propNode p =
      (stream $ streamOfProp p) {nodeId = ncPropNode (C.propertyName p)}


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

  | isCastable streamExprType C.Bool =
    node Bool $ map (extractB . toDyn streamExprType) streamBuffer
               
  | otherwise = 
    node Integer $ map (extractI . toDyn streamExprType) streamBuffer
  
  where

    node :: forall t . Type t -> [t] -> Node
    node t buf = Node { nodeId, nodeDependencies, nodeVars }

      where 
        nodeId = ncNode streamId
        outvar i = LVar (ncMain `ncTimeAnnot` i)

        (e, _dependencies, extNodesLocals) = processExpr t nodeId streamExpr

        outputLocals =
          
          let from i buff =
                case buff of
                  [] -> Map.singleton (outvar i) (LVarDescr t $ Expr e)
                  (b : bs) -> Map.insert (outvar i)
                              (LVarDescr t $ Pre b $ outvar (i + 1))
                              $ from (i + 1) bs

          in from 0 buf
             
        nodeVars = Map.union extNodesLocals outputLocals
        nodeDependencies = nub _dependencies
        nodeOutputs = map outvar [0 .. length buf - 1]
           
--------------------------------------------------------------------------------

-- | Parses the expression
-- | Returns : (expr, new dependencies, new local variables)

--   Important issues :
--   The dependencies list can contain sone duplicates
--   The recursive occurences of the node output is not well labeled

processExpr :: Type t -> NodeId -> C.Expr a -> (Expr t, [NodeId], Map LVar LVarDescr)
processExpr t curNode e = (e', _dependencies s, _lvars s)
  where (e', s) = runState (expr t e) (TransSt Map.empty [] curNode)


expr :: forall t t' . Type t -> C.Expr t' -> Trans (Expr t)

expr t (C.Const t' v) = case t of
  Integer -> return $ Const Integer (extractI $ toDyn t' v)
  Bool    -> return $ Const Bool    (extractB $ toDyn t' v)


expr t (C.Drop _ (fromIntegral -> k :: Int) id) = do
  let node = ncNode id
  selfRef <- (== node) <$> curNode
  let varName = ncMain `ncTimeAnnot` k
  let var = LVar $ if selfRef then varName else ncExt node varName
  when (not selfRef) $ do
    newDep node
    newLocal var $ LVarDescr t $ Ext $ mkGVar node varName
  return $ VarE t var


expr t (C.Local tl _tr id l e)
  | isCastable tl C.Bool = aux Bool
  | otherwise            = aux Integer
    
  where
    aux :: forall a . Type a -> Trans (Expr t)
    aux tl' = do
      l' <- expr tl' l
      newLocal (LVar id) $ LVarDescr tl' $ Expr l'
      expr t e


expr t (C.Var _t' id) = return $ VarE t (LVar id)


expr Bool (C.Op1 op e) = case op of
  C.Not -> expr Bool e >>= return . Op1 Bool Not
  _     -> error "Not handled operator"
  
  
expr Bool (C.Op2 op e1 e2) = case op of
  C.Eq t  -> eqExpr t
  C.Ne t  -> eqExpr t >>= return . Op1 Bool Not
  
  C.Le _  -> binop Bool Integer Le  e1 e2
  C.Lt _  -> binop Bool Integer Lt  e1 e2
  C.Ge _  -> binop Bool Integer Ge  e1 e2
  C.Gt _  -> binop Bool Integer Gt  e1 e2
  C.And   -> binop Bool Bool    And e1 e2
  C.Or    -> binop Bool Bool    Or  e1 e2

  _       -> error "Not handled operator"

  where
    eqExpr :: forall t . C.Type t -> Trans (Expr Bool)
    eqExpr t
      | isCastable t C.Bool = binop Bool Bool    EqB e1 e2
      | otherwise           = binop Bool Integer EqI e1 e2

    
expr Integer (C.Op2 op e1 e2) = case op of
  C.Add _ -> binop Integer Integer Add e1 e2
  C.Sub _ -> binop Integer Integer Sub e1 e2
  C.Mul _ -> binop Integer Integer Mul e1 e2
  _       -> error "Not handled operator"


expr t (C.Op3 (C.Mux _) cond e1 e2) = do
  cond' <- expr Bool cond
  e1'   <- expr t    e1
  e2'   <- expr t    e2
  return $ Ite t cond' e1' e2'



binop :: Type t -> Type targ -> Op2 targ targ t
         -> C.Expr a -> C.Expr b -> Trans (Expr t)
binop t targ op e1 e2 = do
  lhs <- expr targ e1
  rhs <- expr targ e2
  return $ Op2 t op lhs rhs
  
--------------------------------------------------------------------------------

data TransSt = TransSt { _lvars        :: Map LVar LVarDescr
                       , _dependencies :: [NodeId]
                       , _curNode      :: NodeId }
               
type Trans a = State TransSt a

newDep :: NodeId -> Trans ()
newDep d =  modify $ \s -> s { _dependencies = d : _dependencies s }

newLocal :: LVar -> LVarDescr -> Trans ()
newLocal l d  =  modify $ \s -> s { _lvars = Map.insert l d $ _lvars s }

curNode :: Trans NodeId
curNode =  _curNode <$> get

--------------------------------------------------------------------------------
