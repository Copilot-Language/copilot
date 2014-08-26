--------------------------------------------------------------------------------

module Copilot.Kind.Cnub.Translate ( translate ) where
  
import Copilot.Kind.Cnub.Spec
import Copilot.Kind.Misc.Cast
import Copilot.Kind.Misc.Utils
import Control.Monad.State
import Copilot.Kind.CoreUtils.Operators


import qualified Copilot.Core as C
import qualified Data.Map     as Map

import Data.Char (isNumber)

--------------------------------------------------------------------------------

-- 'nc' stands for **naming conventions**

ncStream :: C.Id -> StreamId
ncStream id = "s" ++ show id

-- We assume all local variables have distinct names whatever their scopes are
ncLocal :: C.Name -> StreamId
ncLocal s = "l" ++ dropWhile (not . isNumber) s

ncExternVar :: C.Name -> StreamId
ncExternVar = ("ext-" ++)

ncExternFun :: C.Name -> StreamId
ncExternFun = ("_" ++)

ncUnhandledOp :: String -> UnintId
ncUnhandledOp = id

ncProp :: String -> PropId
ncProp = ("prop-" ++)

ncObserver :: String -> ObserverId
ncObserver = ("obs-" ++)

--------------------------------------------------------------------------------

translate :: C.Spec -> Spec
translate (C.Spec {C.specStreams, C.specProperties, C.specObservers}) =

  let (mainStreams, localStreams, unints) = runTrans $ do
        progStreams <- foldM addStream Map.empty specStreams
        propStreams <- foldM addProp Map.empty specProperties
        obsStreams  <- foldM addObserver Map.empty specObservers
        return $ (progStreams `Map.union` propStreams) `Map.union` obsStreams
  in
  Spec 
  { specStreams    = mainStreams `Map.union` localStreams    
  , specProperties = Map.fromList 
      [(pid, ncProp pid) | pid <- C.propertyName <$> specProperties]
  , specUnints     = unints
  , specObservers  = Map.fromList
      [(oid, ncObserver oid) | oid <- C.observerName <$> specObservers] }
  
  where
    
    addStream acc cs = do
      s <- stream cs
      return $ Map.insert (ncStream $ C.streamId cs) s acc
      
    addProp acc p = do
      s <- prop p
      return $ Map.insert (ncProp $ C.propertyName p) s acc
      
    addObserver acc obs = do
      s <- observer obs
      return $ Map.insert (ncObserver $ C.observerName obs) s acc
    



stream :: C.Stream -> Trans Stream
stream (C.Stream {C.streamBuffer, C.streamExpr, C.streamExprType}) =
  casting streamExprType $ \t -> do
  e <- expr t streamExpr
  return $ Stream
    { streamBuffer = map (cast t . toDyn streamExprType) streamBuffer
    , streamExpr = e 
    , streamType = t }


prop :: C.Property -> Trans Stream
prop (C.Property {C.propertyExpr}) = do
  e <- expr Bool propertyExpr
  return $ Stream
    { streamBuffer = []
    , streamExpr = e
    , streamType = Bool }


observer :: C.Observer -> Trans Stream
observer (C.Observer {C.observerExpr, C.observerExprType} ) = 
  casting observerExprType $ \t' -> do
    e <- expr t' observerExpr
    return $ Stream
      { streamBuffer = []
      , streamExpr =  e
      , streamType = t' }




expr :: Type t' -> C.Expr t -> Trans (Expr t')

expr t (C.Const t0 v) = return $ Const t (cast t $ toDyn t0 v)

expr t (C.Drop _ k id) = return $ 
  Drop t (fromInteger . toInteger $ k) (ncStream id)

expr t (C.Local ta _ name ea eb) = casting ta $ \ta' -> do
  ea' <- expr ta' ea
  newLocalStream (ncLocal name) (Stream [] ta' ea')
  expr t eb
  
expr t (C.Var _ name) = return $ Drop t 0 (ncLocal name)

expr t (C.ExternVar _ name _) = do
  let name' = ncExternVar name
  newUnint name' (UnintFun t [])
  return (Unint t name' [])
  
expr t (C.ExternFun ta name args _ _) = do
  let name' = ncExternFun name
  newUnint name' unintDescr
  mapM trArg args >>= return . Unint t name'
  
  where
    trArg (C.UExpr {C.uExprExpr, C.uExprType}) = casting uExprType $ \ta ->
      U <$> expr ta uExprExpr
  
    unintDescr =  
      let argType (C.UExpr {C.uExprType}) = casting uExprType U
      in casting ta $ \ta' ->
        UnintFun ta' (map argType args)
             
expr t (C.ExternArray _ tb name _ ind _ _) = casting tb $ \tb' -> do
  let name' = ncExternFun name
  newUnint name' (UnintFun tb' [U Integer])
  ind' <- U <$> expr Integer ind
  return $ Unint t name' [ind']
  
expr t (C.Op1 op e) = handleOp1
  t (op, e) expr undefined undefined
  
  where notHandled (UnhandledOp1 opName ta tb) = do
          e' <- U <$> expr ta e
          let opName' = ncUnhandledOp opName
          newUnint opName' (UnintFun tb [U ta])
          return $ Unint t opName' [e']
          

expr t (C.Op2 op e1 e2) = handleOp2
  t (op, e1, e2) expr notHandled Op2 (Op1 Bool Not)
  
  where notHandled (UnhandledOp2 opName ta tb tc) = do
          e1' <- U <$> expr ta e1
          e2' <- U <$> expr tb e2
          let opName' = ncUnhandledOp opName
          newUnint opName' $ UnintFun tc [U ta, U tb]
          return $ Unint t opName' [e1', e2']

expr t (C.Op3 (C.Mux _) cond e1 e2) = do
  cond' <- expr Bool cond
  e1'   <- expr t    e1
  e2'   <- expr t    e2
  return $ Ite t cond' e1' e2'
          
--------------------------------------------------------------------------------

type Trans a = State TransST a

data TransST = TransST
  { _localStreams :: Map StreamId Stream
  , _unints       :: Map UnintId UnintFun }

newLocalStream id s = modify $ \st -> 
  st { _localStreams = Map.insert id s $ _localStreams st }

newUnint id args = modify $ \st ->
  st { _unints = Map.insert id args $ _unints st }

runTrans :: Trans a -> (a, Map StreamId Stream, Map UnintId UnintFun)
runTrans m = (a, _localStreams, _unints)
  where (a, TransST {_localStreams, _unints}) = 
          runState m $ TransST Map.empty Map.empty

--------------------------------------------------------------------------------
  