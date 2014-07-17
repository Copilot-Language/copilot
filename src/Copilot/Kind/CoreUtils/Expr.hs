--------------------------------------------------------------------------------

module Copilot.Kind.CoreUtils.Expr where

import Copilot.Core
import Data.Monoid
import Data.List (concatMap)

--------------------------------------------------------------------------------

foldCExpr :: forall m a . (Monoid m) => 
             (forall t . Expr t -> m) -> Expr a -> m
             
foldCExpr f = fld
  where 
    fld :: forall a . Expr a -> m
    fld e = f e <> case e of
      (Local _ _ _ ea eb)          -> fld ea <> fld eb
      (ExternFun _ _ args _ _)     -> foldl fldArgs mempty args
      (ExternArray _ _ _ _ ea _ _) -> fld ea
      (Op1 _ ea)                   -> fld ea
      (Op2 _ ea eb)                -> fld ea <> fld eb 
      (Op3 _ ea eb ec)             -> fld ea <> fld eb <> fld ec
      _                            -> mempty
      
    fldArgs :: m -> UExpr -> m
    fldArgs m (UExpr {uExprExpr}) = m <> fld uExprExpr
    
--------------------------------------------------------------------------------
 
foldCSpecExprs :: forall m . (Monoid m) => 
                  (forall t . Expr t -> m) -> Spec -> m

foldCSpecExprs f spec = 
  mconcat $ 
    map streamExpr (specStreams spec)
    ++ map observerExpr (specObservers spec)
    ++ map triggerExpr (specTriggers spec)
    ++ map propertyExpr (specProperties spec)
    
  where streamExpr    (Stream   {streamExpr})   = foldCExpr f streamExpr
        observerExpr  (Observer {observerExpr}) = foldCExpr f observerExpr
        triggerExpr   (Trigger  {triggerGuard}) = foldCExpr f triggerGuard
        propertyExpr  (Property {propertyExpr}) = foldCExpr f propertyExpr
        
--------------------------------------------------------------------------------

addBoundCheckingProps :: Spec -> (Spec, [String])
addBoundCheckingProps spec = 
  ( spec {specProperties = properties}
  , map propertyName properties )
  where
    properties = foldCSpecExprs checkExpr spec
  
    checkExpr (ExternArray tind _ name size ind _ (Just tag)) =
      let propertyName = "_bounds_" ++ show name ++ "_" ++ show tag ++ "_"
          propertyExpr = Op2 And 
            (Op2 (Le tind) (Const tind 0) ind)
            (Op2 (Lt tind) ind (Const tind (fromInteger . toInteger $ size)))
          
      in [Property propertyName propertyExpr]
    
    checkExpr _ = mempty

--------------------------------------------------------------------------------
