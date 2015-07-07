--------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes, NamedFieldPuns, ScopedTypeVariables, GADTs #-}

module Copilot.Kind.IL.Translate ( translate, getVars ) where

import Copilot.Kind.IL.Spec
import Copilot.Kind.Misc.Cast
import Copilot.Kind.Misc.Utils

import qualified Copilot.Core as C
import Copilot.Kind.CoreUtils.Operators
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad.Writer
import Control.Monad.State

import Data.Char

--------------------------------------------------------------------------------

-- 'nc' stands for **naming conventions**
ncSeq :: C.Id -> SeqId
ncSeq = printf "s%d"

-- We assume all local variables have distinct names whatever their scopes are
ncLocal :: C.Name -> SeqId
ncLocal s = "l" ++ dropWhile (not . isNumber) s

ncExternVar :: C.Name -> SeqId
ncExternVar n = "ext_" ++ n

ncExternFun :: C.Name -> SeqId
ncExternFun n = "_" ++ n

ncUnhandledOp :: String -> String
ncUnhandledOp = id

--------------------------------------------------------------------------------

-- | Translates a Copilot specification to an IL specification

translate :: C.Spec -> IL
translate (C.Spec {C.specStreams, C.specProperties}) = runTrans $ do

  let mainSeqs = map seqDescr specStreams
  let modelInit = concatMap streamInit specStreams

  mainConstraints <- mapM streamRec specStreams
  properties <- Map.fromList <$>
    (forM specProperties $
      \(C.Property {C.propertyName, C.propertyExpr}) -> do
        e' <- expr Bool propertyExpr
        return (propertyName, e'))

  localConstraints <- getLocalVarsConstraints

  return IL
    { modelInit
    , modelRec = mainConstraints ++ localConstraints
    , properties
    , depth = maximum $
      [0] ++ map (\(C.Stream { C.streamBuffer }) -> length streamBuffer) specStreams
    }

seqDescr :: C.Stream -> SeqDescr
seqDescr (C.Stream { C.streamId, C.streamExprType }) =
  casting streamExprType $ SeqDescr $ ncSeq streamId

streamInit :: C.Stream -> [Constraint]
streamInit (C.Stream { C.streamId       = id
                     , C.streamBuffer   = b :: [val]
                     , C.streamExprType = ty}) =

  zipWith initConstraint [0,1..] b
  where
    initConstraint :: Integer -> val -> Constraint
    initConstraint p v = casting ty $ \ty' ->
      Op2 Bool Eq
        (SVal ty' (ncSeq id) (Fixed p))
        (Const ty' $ cast ty' $ toDyn ty v)

streamRec :: C.Stream -> Trans Constraint
streamRec (C.Stream { C.streamId       = id
                    , C.streamExpr     = e
                    , C.streamBuffer   = b
                    , C.streamExprType = ty})
  = let depth = length b in casting ty $ \ty' ->
    do
      e' <- expr ty' e
      return $ Op2 Bool Eq
         (SVal ty' (ncSeq id) (_n_plus depth)) e'

--------------------------------------------------------------------------------

expr :: Type t -> C.Expr a -> Trans (Expr t)

-- TODO(chathhorn): blegh clean up
expr t (C.Const ct@C.Double v)
  | v >= 0 = return $ Const t (cast t $ toDyn ct v)
  | otherwise = return $ Op1 t Neg (Const t (cast t $ toDyn ct (-v)))
expr t (C.Const ct@C.Float v)
  | v >= 0 = return $ Const t (cast t $ toDyn ct v)
  | otherwise = return $ Op1 t Neg (Const t (cast t $ toDyn ct (-v)))
expr t (C.Const ct@C.Int8 v)
  | v >= 0 = return $ Const t (cast t $ toDyn ct v)
  | otherwise = return $ Op1 t Neg (Const t (cast t $ toDyn ct (-v)))
expr t (C.Const ct@C.Int16 v)
  | v >= 0 = return $ Const t (cast t $ toDyn ct v)
  | otherwise = return $ Op1 t Neg (Const t (cast t $ toDyn ct (-v)))
expr t (C.Const ct@C.Int32 v)
  | v >= 0 = return $ Const t (cast t $ toDyn ct v)
  | otherwise = return $ Op1 t Neg (Const t (cast t $ toDyn ct (-v)))
expr t (C.Const ct@C.Int64 v)
  | v >= 0 = return $ Const t (cast t $ toDyn ct v)
  | otherwise = return $ Op1 t Neg (Const t (cast t $ toDyn ct (-v)))

expr t (C.Const ct v) = return $ Const t (cast t $ toDyn ct v)

expr t (C.Drop _ k id) = return $ SVal t (ncSeq id) (_n_plus k)

expr t (C.Local ta _ name ea eb) = casting ta $ \ta' -> do
  ea' <- expr ta' ea
  newLocalVar
    (SeqDescr (ncLocal name) ta')
    (Op2 Bool Eq (SVal ta' (ncLocal name) _n_) ea')
  expr t eb

expr t (C.Var _ name) = return $ SVal t (ncLocal name) _n_

expr t (C.ExternVar ta name _) = casting ta $ \ta' -> do
  newExternVar (SeqDescr (ncExternVar name) ta')
  return $ SVal t (ncExternVar name) _n_

expr t (C.ExternFun _ name args _ _) = do
  args' <- mapM trArg args
  return $ FunApp t (ncExternFun name) args'
  where
    trArg (C.UExpr {C.uExprExpr, C.uExprType}) = casting uExprType $ \ta ->
      U <$> expr ta uExprExpr

-- Arrays and functions are treated the same way
expr t (C.ExternArray _ tb name _ ind _ _) = casting tb $ \_ -> do
  ind' <- U <$> expr Integer ind
  return $ FunApp t (ncExternFun name) [ind']

expr t (C.Op1 op e) = handleOp1 t (op, e) expr notHandled Op1
  where notHandled (UnhandledOp1 opName ta _) = do
          e' <- U <$> expr ta e
          return $ FunApp t (ncUnhandledOp opName) [e']

expr t (C.Op2 op e1 e2) = handleOp2 t (op, e1, e2) expr notHandled Op2 (Op1 Bool Not)
  where notHandled (UnhandledOp2 opName ta tb _) = do
          e1' <- U <$> expr ta e1
          e2' <- U <$> expr tb e2
          return $ FunApp t (ncUnhandledOp opName) [e1', e2']

expr t (C.Op3 (C.Mux _) cond e1 e2) = do
  cond' <- expr Bool cond
  e1'   <- expr t    e1
  e2'   <- expr t    e2
  return $ Ite t cond' e1' e2'

--------------------------------------------------------------------------------

mkVarName :: String -> [U Expr] -> String
mkVarName name args = name ++ "_" ++ concatMap argToString args

argToString :: U Expr -> String
argToString (U (Const Integer x)) = show x
argToString (U (Op2 _ Add e1 e2)) = argToString (U e1) ++ argToString (U e2)
argToString (U _) = error "what"

data TransST = TransST
  { _localSeqs :: [SeqDescr]
  , _localVarsConstraints :: [Constraint]
  }

type Trans a = State TransST a

newExternVar :: SeqDescr -> Trans ()
newExternVar d = modify $ \st -> st {_localSeqs = d : _localSeqs st}

newLocalVar :: SeqDescr -> Constraint -> Trans ()
newLocalVar d c = do
  modify $ \st -> st {_localSeqs = d : _localSeqs st}
  modify $ \st -> st {_localVarsConstraints = c : _localVarsConstraints st}

getVars :: [Constraint] -> [VarDescr]
getVars = nubBy' (compare `on` varName) . concatMap getExprVars

getExprVars :: Expr a -> [VarDescr]
getExprVars (Const _ _) = []
getExprVars (Ite _ e1 e2 e3) = getExprVars e1 ++ getExprVars e2 ++ getExprVars e3
getExprVars (Op1 _ _ e) = getExprVars e
getExprVars (Op2 _ _ e1 e2) = getExprVars e1 ++ getExprVars e2
getExprVars (SVal t seq (Fixed i)) = [VarDescr (seq ++ "_" ++ show i) t]
getExprVars (SVal t seq (Var i)) = [VarDescr (seq ++ "_n" ++ show i) t]
getExprVars (FunApp _ _ args) = concatMap getUExprVars args

getUExprVars :: U Expr -> [VarDescr]
getUExprVars (U e) = getExprVars e

getLocalVarsConstraints :: Trans [Constraint]
getLocalVarsConstraints = _localVarsConstraints <$> get

runTrans :: Trans a -> a
runTrans m = evalState m $ TransST [] []

--------------------------------------------------------------------------------
