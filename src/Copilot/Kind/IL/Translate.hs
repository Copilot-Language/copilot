--------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes, NamedFieldPuns, ScopedTypeVariables, GADTs,
             LambdaCase #-}

module Copilot.Kind.IL.Translate ( translate ) where

import Copilot.Kind.IL.Spec

import qualified Copilot.Core as C

import qualified Data.Map.Strict as Map

import Control.Applicative ((<$>), (<*))
import Control.Monad.State

import Data.Char

import Text.Printf

import GHC.Float (float2Double)

--------------------------------------------------------------------------------

-- 'nc' stands for naming convention.
ncSeq :: C.Id -> SeqId
ncSeq = printf "s%d"

-- We assume all local variables have distinct names whatever their scopes.
ncLocal :: C.Name -> SeqId
ncLocal s = "l" ++ dropWhile (not . isNumber) s

ncExternVar :: C.Name -> SeqId
ncExternVar n = "ext_" ++ n

ncExternFun :: C.Name -> SeqId
ncExternFun n = "_" ++ n

ncUnhandledOp :: String -> String
ncUnhandledOp = id

ncFresh :: Integer -> SeqId
ncFresh n = "f" ++ show n

--------------------------------------------------------------------------------

-- | Translates a Copilot specification to an IL specification

translate :: C.Spec -> IL
translate (C.Spec {C.specStreams, C.specProperties}) = runTrans $ do

  let modelInit = concatMap streamInit specStreams

  mainConstraints <- mapM streamRec specStreams

  localConstraints <- getLocalConstraints
  properties <- Map.fromList <$>
    forM specProperties
      (\(C.Property {C.propertyName, C.propertyExpr}) -> do
        e' <- expr propertyExpr
        propConds <- getLocalConstraints
        return (propertyName, (propConds, e')))

  return IL
    { modelInit
    , modelRec = mainConstraints ++ localConstraints
    , properties
    , inductive = not $ null specStreams
    }

bound :: Expr -> C.Type a -> Trans ()
bound s t = case t of
  C.Int8    -> bound' C.Int8
  C.Int16   -> bound' C.Int16
  C.Int32   -> bound' C.Int32
  C.Int64   -> bound' C.Int64
  C.Word8   -> bound' C.Word8
  C.Word16  -> bound' C.Word16
  C.Word32  -> bound' C.Word32
  C.Word64  -> bound' C.Word64
  _         -> return ()
  where bound' :: (Bounded a, Integral a) => C.Type a -> Trans ()
        bound' t = localConstraint (Op2 Bool And
            (Op2 Bool Le (trConst t minBound) s)
            (Op2 Bool Ge (trConst t maxBound) s))

streamInit :: C.Stream -> [Expr]
streamInit (C.Stream { C.streamId       = id
                     , C.streamBuffer   = b :: [val]
                     , C.streamExprType = t }) =
  zipWith initConstraint [0..] b
  where initConstraint :: Integer -> val -> Expr
        initConstraint p v = Op2 Bool Eq
          (SVal (trType t) (ncSeq id) (Fixed p))
          $ trConst t v

streamRec :: C.Stream -> Trans Expr
streamRec (C.Stream { C.streamId       = id
                    , C.streamExpr     = e
                    , C.streamBuffer   = b
                    , C.streamExprType = t })
  = do
  let s = SVal (trType t) (ncSeq id) (_n_plus $ length b)
  bound s t
  e' <- expr e
  return $ Op2 Bool Eq s e'

--------------------------------------------------------------------------------

expr :: C.Expr a -> Trans Expr

expr (C.Const t v) = return $ trConst t v

expr (C.Label _ _ e) = expr e

expr (C.Drop t k id) = return $ SVal (trType t) (ncSeq id) (_n_plus k)

expr (C.Local ta _ name ea eb) = do
  ea' <- expr ea
  localConstraint (Op2 Bool Eq (SVal (trType ta) (ncLocal name) _n_) ea')
  expr eb

expr (C.Var t name) = return $ SVal (trType t) (ncLocal name) _n_

expr (C.ExternVar t name _) = bound s t >> return s
  where s = SVal (trType t) (ncExternVar name) _n_

expr (C.ExternFun t name args _ _) = do
  args' <- mapM trArg args
  let s = FunApp (trType t) (ncExternFun name) args'
  bound s t
  return s
  where trArg (C.UExpr {C.uExprExpr}) = expr uExprExpr

-- Arrays and functions are treated the same way
expr (C.ExternArray ta tb name _ ind _ _) =
  expr (C.ExternFun tb name [C.UExpr ta ind] Nothing Nothing)

expr (C.Op1 (C.Sign ta) e) = case ta of
  C.Int8   -> trSign ta e
  C.Int16  -> trSign ta e
  C.Int32  -> trSign ta e
  C.Int64  -> trSign ta e
  C.Float  -> trSign ta e
  C.Double -> trSign ta e
  _        -> expr $ C.Const ta 1
  where trSign :: (Ord a, Num a) => C.Type a -> C.Expr a -> Trans Expr
        trSign ta e =
          expr (C.Op3 (C.Mux ta)
            (C.Op2 (C.Lt ta) e (C.Const ta 0))
            (C.Const ta (-1))
            (C.Op3 (C.Mux ta)
              (C.Op2 (C.Gt ta) e (C.Const ta 0))
              (C.Const ta 1)
              (C.Const ta 0)))
expr (C.Op1 (C.Sqrt _) e) = do
  e' <- expr e
  return $ Op2 Real Pow e' (ConstR 0.5)
expr (C.Op1 (C.Cast _ _) e) = expr e
expr (C.Op1 op e) = do
  e' <- expr e
  return $ Op1 t' op' e'
  where (op', t') = trOp1 op

expr (C.Op2 (C.Ne t) e1 e2) = do
  e1' <- expr e1
  e2' <- expr e2
  return $ Op1 Bool Not (Op2 t' Eq e1' e2')
  where t' = trType t

expr (C.Op2 op e1 e2) = do
  e1' <- expr e1
  e2' <- expr e2
  return $ Op2 t' op' e1' e2'
  where (op', t') = trOp2 op

expr (C.Op3 (C.Mux t) cond e1 e2) = do
  cond' <- expr cond
  e1'   <- expr e1
  e2'   <- expr e2
  f     <- fresh
  let s = SVal (trType t) f _n_
  localConstraint $ Op2 Bool Or (Op1 Bool Not cond') (Op2 Bool Eq s e1')
  localConstraint $ Op2 Bool Or cond' (Op2 Bool Eq s e2')
  return s

trConst :: C.Type a -> a -> Expr
trConst t v = case t of
  C.Bool   -> ConstB v
  C.Int8   -> negifyI v
  C.Int16  -> negifyI v
  C.Int32  -> negifyI v
  C.Int64  -> negifyI v
  C.Word8  -> negifyI v
  C.Word16 -> negifyI v
  C.Word32 -> negifyI v
  C.Word64 -> negifyI v
  C.Float  -> negifyR (float2Double v)
  C.Double -> negifyR v
  where negifyR :: Double -> Expr
        negifyR v
            | v >= 0    = ConstR v
            | otherwise = Op1 Real Neg $ ConstR $ negate $ v
        negifyI :: Integral a => a -> Expr
        negifyI v
            | v >= 0    = ConstI $ toInteger v
            | otherwise = Op1 Integer Neg $ ConstI $ negate $ toInteger v

trOp1 :: C.Op1 a b -> (Op1, Type)
trOp1 = \case
  C.Not     -> (Not, Bool)
  C.Abs t   -> (Abs, trType t)
  -- C.Sign t  ->
  -- C.Recip t ->
  C.Exp t   -> (Exp, trType t)
  -- C.Sqrt t  ->
  C.Log t   -> (Log, trType t)
  C.Sin t   -> (Sin, trType t)
  C.Tan t   -> (Tan, trType t)
  C.Cos t   -> (Cos, trType t)
  C.Asin t  -> (Asin, trType t)
  C.Atan t  -> (Atan, trType t)
  C.Acos t  -> (Acos, trType t)
  C.Sinh t  -> (Sinh, trType t)
  C.Tanh t  -> (Tanh, trType t)
  C.Cosh t  -> (Cosh, trType t)
  C.Asinh t -> (Asinh, trType t)
  C.Atanh t -> (Atanh, trType t)
  C.Acosh t -> (Acosh, trType t)
  -- C.BwNot t ->
  -- C.Cast t  ->
  _ -> error "Unsupported unary operator in input." -- TODO(chathhorn)

trOp2 :: C.Op2 a b c -> (Op2, Type)
trOp2 = \case
  C.And          -> (And, Bool)
  C.Or           -> (Or, Bool)

  C.Add t        -> (Add, trType t)
  C.Sub t        -> (Sub, trType t)
  C.Mul t        -> (Mul, trType t)

  C.Mod t        -> (Mod, trType t)
  -- C.Div t        ->

  C.Fdiv t       -> (Fdiv, trType t)

  C.Pow t        -> (Pow, trType t)
  -- C.Logb t       ->

  C.Eq t         -> (Eq, trType t)
  -- C.Ne t         ->

  C.Le t         -> (Le, trType t)
  C.Ge t         -> (Ge, trType t)
  C.Lt t         -> (Lt, trType t)
  C.Gt t         -> (Gt, trType t)

  -- C.BwAnd t      ->
  -- C.BwOr t       ->
  -- C.BwXor t      ->
  -- C.BwShiftL t _ ->
  -- C.BwShiftR t _ ->

  _ -> error "Unsupported binary operator in input." -- TODO(chathhorn)

trType :: C.Type a -> Type
trType = \case
  C.Bool   -> Bool
  C.Int8   -> Integer
  C.Int16  -> Integer
  C.Int32  -> Integer
  C.Int64  -> Integer
  C.Word8  -> Integer
  C.Word16 -> Integer
  C.Word32 -> Integer
  C.Word64 -> Integer
  C.Float  -> Real
  C.Double -> Real

--------------------------------------------------------------------------------

data TransST = TransST
  { localConstraints :: [Expr]
  , nextFresh        :: Integer
  }

type Trans = State TransST

localConstraint :: Expr -> Trans ()
localConstraint c =
  modify $ \st -> st {localConstraints = c : localConstraints st}

fresh :: Trans String
fresh = do
  modify $ \st -> st {nextFresh = nextFresh st + 1}
  n <- nextFresh <$> get
  return $ ncFresh n

getLocalConstraints :: Trans [Expr]
getLocalConstraints = (localConstraints <$> get) <*
  (modify $ \st -> st {localConstraints = []})

runTrans :: Trans a -> a
runTrans m = evalState m $ TransST [] 0

--------------------------------------------------------------------------------
