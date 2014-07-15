--------------------------------------------------------------------------------

module Copilot.Kind.Misc.CoreOperators where

import qualified Copilot.Core as C
import Copilot.Kind.Misc.Cast
import Copilot.Kind.Misc.Type
import Copilot.Kind.Misc.Operators

import Copilot.Kind.Misc.Error as Err

--------------------------------------------------------------------------------

-- | Some high level utilities to translate a Copilot operator in a standard way
-- | The unhandled operators are monomorphic, and thei names are labeled so that
-- | each name corresponds to a unique uninterpreted function with a monomorphic
-- | type

data UnhandledOp1 = forall a b . 
  UnhandledOp1 String (Type a) (Type b)
  
data UnhandledOp2 = forall a b c . 
  UnhandledOp2 String (Type a) (Type b) (Type c)

handleOp1 :: 
  -- 'm' is the monad in which the computation is made
  -- 'resT' the desires return type
  forall m expr _a _b resT. (Monad m) =>
  -- Desired return type
  (Type resT) ->
  -- Input Op Expr
  (C.Op1 _a _b, C.Expr _a) ->
  -- Monadic function to translate an expression
  (forall t t'. Type t -> C.Expr t' -> m (expr t)) ->
  -- To deal with a not handled operator
  (UnhandledOp1 -> m (expr resT)) ->
  -- The Op1 constructor of the 'expr' type
  (forall t a . Type t -> Op1 a t -> expr a -> expr t) ->
  -- The final function
  m (expr resT)
  

handleOp1 resT (op, e) handleExpr notHandledF mkOp = case op of

  C.Not      -> boolOp Not (handleExpr Bool e)
  
  -- Numeric operators
  C.Abs ta   -> notHandled ta tagReal "abs"
  C.Sign ta  -> notHandled ta tagReal "sign"
  
  -- Fractional operators
  C.Recip ta -> notHandled ta noTag "recip"
  
  -- Floating operators
  C.Exp ta   -> notHandled ta noTag "exp"
  C.Sqrt ta  -> notHandled ta noTag "sqrt"
  C.Log ta   -> notHandled ta noTag "log"
  C.Sin ta   -> notHandled ta noTag "sin"
  C.Tan ta   -> notHandled ta noTag "tan"
  C.Cos ta   -> notHandled ta noTag "cos"
  C.Asin ta  -> notHandled ta noTag "asin"
  C.Atan ta  -> notHandled ta noTag "atan"
  C.Acos ta  -> notHandled ta noTag "acos"
  C.Sinh ta  -> notHandled ta noTag "sinh"
  C.Tanh ta  -> notHandled ta noTag "tanh"
  C.Cosh ta  -> notHandled ta noTag "cosh"
  C.Asinh ta -> notHandled ta noTag "asinh"
  C.Atanh ta -> notHandled ta noTag "atanh"
  C.Acosh ta -> notHandled ta noTag "acosh"
  
  -- Bitwise operators.
  C.BwNot ta -> notHandled ta noTag "bwnot"
  
  -- Casting operator.
  C.Cast ta tb -> castTo ta tb
  
  where
    boolOp :: Op1 a Bool -> m (expr a) -> m (expr resT)
    boolOp op e1'= case resT of
      Bool -> e1' >>= return . mkOp resT op
      _    -> Err.impossible typeErrMsg
       
    -- Casting from Integer (Only possible solution)
    castTo :: C.Type cta -> C.Type ctb -> m (expr resT)
    castTo ta tb = casting tb $ \tb' -> case (tb', resT) of
      (Integer, Integer) -> handleExpr Integer e
      (Real, Real)       -> notHandled ta noTag "int2real"
      _                  -> Err.impossible typeErrMsg
      
    notHandled :: 
      C.Type a -> (forall c . Type c -> String) -> String -> m (expr resT)
    notHandled ta typeTag s = casting ta $ \ta' ->
      notHandledF $ UnhandledOp1 (typeTag ta' ++ s) ta' resT
    
    tagReal Real = "f"
    tagReal _    = ""
    
    noTag _ = ""
      
      
--------------------------------------------------------------------------------

-- See the 'handleOp1' function for documentation
handleOp2 :: 
  forall m expr _a _b _c resT . (Monad m) =>
  (Type resT) ->
  (C.Op2 _a _b _c, C.Expr _a, C.Expr _b) -> 
  (forall t t'. Type t -> C.Expr t' -> m (expr t)) ->
  (UnhandledOp2 -> m (expr resT)) ->
  (forall t a b . Type t -> Op2 a b t -> expr a -> expr b -> expr t) ->
  m (expr resT)


handleOp2 resT (op, e1, e2) handleExpr notHandledF mkOp = case op of 

  C.And        -> boolConnector And
  C.Or         -> boolConnector Or
  
  -- Numeric operators
  C.Add _      -> numOp Add
  C.Sub _      -> numOp Sub
  C.Mul _      -> numOp Mul
  
  -- Integral operators.
  C.Mod ta    -> notHandled ta "mod"
  C.Div ta    -> notHandled ta "div"
  
  -- Fractional operators.
  C.Fdiv ta   -> notHandled ta "fdiv"
  
  -- Floating operators.
  C.Pow ta    -> notHandled ta "pow"
  C.Logb ta   -> notHandled ta "logb"
  
  -- Equality operators.
  C.Eq ta     -> eqOp ta Eq
  C.Ne ta     -> eqOp ta Ne
  
  -- Relational operators.
  C.Le ta     -> numComp ta Le
  C.Ge ta     -> numComp ta Ge
  C.Lt ta     -> numComp ta Lt
  C.Gt ta     -> numComp ta Gt
  
  -- Bitwise operators.
  C.BwAnd ta          -> notHandled ta "bwand"
  C.BwOr ta           -> notHandled ta "bwor"
  C.BwXor ta          -> notHandled ta "bwxor"
  
  -- In fact, '_tb' is ignored caused it can only 
  -- be casted to 'Integer', like 'ta'
  C.BwShiftL ta _tb   -> notHandled ta "bwshiftl"
  C.BwShiftR ta _tb   -> notHandled ta "bwshiftr"
  
  where  
  
    boolOp :: Op2 a b Bool -> expr a -> expr b -> expr resT
    boolOp op e1' e2' = case resT of
      Bool -> mkOp resT op e1' e2'
      _    -> Err.impossible typeErrMsg
    
    boolConnector :: Op2 Bool Bool Bool -> m (expr resT)
    boolConnector op = do 
     e1' <- handleExpr Bool e1
     e2' <- handleExpr Bool e2
     return $ boolOp op e1' e2'
    
    eqOp :: C.Type cta -> (forall a . Op2 a a Bool) -> m (expr resT)
    eqOp ta op = casting ta $ \ta' -> do
      e1' <-  handleExpr ta' e1
      e2' <-  handleExpr ta' e2
      return $ boolOp op e1' e2'
      
    numOp :: (forall num . (Num num) => Op2 num num num) -> m (expr resT)
    numOp op = case resT of
      Integer -> do 
        e1' <- handleExpr Integer e1
        e2' <- handleExpr Integer e2
        return $ mkOp resT op e1' e2'
             
      Real -> do
        e1' <- handleExpr Real e1
        e2' <- handleExpr Real e2
        return $ mkOp resT op e1' e2'
        
      _ -> Err.impossible typeErrMsg
      
    numComp :: 
      C.Type cta -> 
      (forall num . (Num num) => Op2 num num Bool) -> m (expr resT)
    numComp ta op = casting ta $ \case
      Integer -> do 
        e1' <- handleExpr Integer e1
        e2' <- handleExpr Integer e2
        return $ boolOp op e1' e2'
      Real -> do
        e1' <- handleExpr Real e1
        e2' <- handleExpr Real e2
        return $ boolOp op e1' e2'      
      _       -> Err.impossible typeErrMsg
      
    notHandled :: forall a . C.Type a -> String -> m (expr resT)
    notHandled ta s = casting ta $ \ta' -> 
      notHandledF (UnhandledOp2 s ta' ta' ta')

--------------------------------------------------------------------------------

typeErrMsg :: String
typeErrMsg = "Unexpected type error in 'Misc.CoreOperators'"

--------------------------------------------------------------------------------

