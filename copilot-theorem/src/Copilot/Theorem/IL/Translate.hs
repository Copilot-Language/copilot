{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Translate Copilot specifications into IL specifications.
module Copilot.Theorem.IL.Translate ( translate, translateWithBounds ) where

import Copilot.Theorem.IL.Spec

import qualified Copilot.Core as C

import qualified Data.Map.Strict as Map

#if MIN_VERSION_base(4,19,0)
import Control.Monad       (forM, liftM2, when)
#endif
import Control.Monad.State

import Data.Char
import Data.List (find)

import Text.Printf

import GHC.Float (float2Double)

import Data.Typeable (Typeable)
import Prelude hiding (id)

-- 'nc' stands for naming convention.
ncSeq :: C.Id -> SeqId
ncSeq = printf "s%d"

-- We assume all local variables have distinct names whatever their scopes.
ncLocal :: C.Name -> SeqId
ncLocal s = "l" ++ dropWhile (not . isNumber) s

ncExternVar :: C.Name -> SeqId
ncExternVar n = "ext_" ++ n

ncMux :: Integer -> SeqId
ncMux n = "mux" ++ show n

-- | Translate a Copilot specification to an IL specification.
translate :: C.Spec -> IL
translate = translate' False

-- | Translate a Copilot specification to an IL specification, adding
-- constraints for limiting the values of numeric expressions to known bounds
-- based on their specific types (only for integers or natural numbers).
translateWithBounds :: C.Spec -> IL
translateWithBounds = translate' True

translate' :: Bool -> C.Spec -> IL
translate' b (C.Spec {C.specStreams, C.specProperties}) = runTrans b $ do

  let modelInit = concatMap streamInit specStreams

  mainConstraints <- mapM streamRec specStreams

  localConstraints <- popLocalConstraints
  properties <- Map.fromList <$>
    forM specProperties
      (\(C.Property {C.propertyName, C.propertyProp}) -> do
        -- Soundness note: it is OK to call `extractProp` here to drop the
        -- quantifier from the proposition `propertyProp`. This is because we
        -- IL translation always occurs within the context of a function that
        -- returns a `Proof`, and these `Proof` functions are always careful to
        -- use `Prover`s that respect the propositions's quantifier.
        e' <- expr (C.extractProp propertyProp)
        propConds <- popLocalConstraints
        return (propertyName, (propConds, e')))

  return IL
    { modelInit
    , modelRec = mainConstraints ++ localConstraints
    , properties
    , inductive = not $ null specStreams
    }

bound :: Expr -> C.Type a -> Trans ()
bound s sType = case sType of
  C.Int8    -> bound' C.Int8
  C.Int16   -> bound' C.Int16
  C.Int32   -> bound' C.Int32
  C.Int64   -> bound' C.Int64
  C.Word8   -> bound' C.Word8
  C.Word16  -> bound' C.Word16
  C.Word32  -> bound' C.Word32
  C.Word64  -> bound' C.Word64
  _         -> return ()
  where
    bound' :: (Bounded a, Integral a) => C.Type a -> Trans ()
    bound' t = do
      b <- addBounds <$> get
      when b $ localConstraint (Op2 Bool And
        (Op2 Bool Le (trConst t minBound) s)
        (Op2 Bool Ge (trConst t maxBound) s))

streamInit :: C.Stream -> [Expr]
streamInit (C.Stream { C.streamId       = id
                     , C.streamBuffer   = b :: [val]
                     , C.streamExprType = t }) =
  zipWith initConstraint [0..] b
  where
    initConstraint :: Integer -> val -> Expr
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

expr :: Typeable a => C.Expr a -> Trans Expr

expr (C.Const t v) = return $ trConst t v

expr (C.Label _ _ e) = expr e

expr (C.Drop t k id) = return $ SVal (trType t) (ncSeq id) (_n_plus k)

expr (C.Local ta _ name ea eb) = do
  ea' <- expr ea
  localConstraint (Op2 Bool Eq (SVal (trType ta) (ncLocal name) _n_) ea')
  expr eb

expr (C.Var t name) = return $ SVal (trType t) (ncLocal name) _n_

expr (C.ExternVar t name _) = bound s t >> return s
  where
    s = SVal (trType t) (ncExternVar name) _n_

expr (C.Op1 (C.Sign ta) e) = case ta of
  C.Int8   -> trSign ta e
  C.Int16  -> trSign ta e
  C.Int32  -> trSign ta e
  C.Int64  -> trSign ta e
  C.Float  -> trSign ta e
  C.Double -> trSign ta e
  _        -> expr $ C.Const ta 1
  where
    trSign :: (Typeable a, Ord a, Num a) => C.Type a -> C.Expr a -> Trans Expr
    trSign tb e' =
      expr (C.Op3 (C.Mux tb)
        (C.Op2 (C.Lt tb) e' (C.Const tb 0))
        (C.Const tb (-1))
        (C.Op3 (C.Mux tb)
          (C.Op2 (C.Gt tb) e' (C.Const tb 0))
          (C.Const tb 1)
          (C.Const tb 0)))
expr (C.Op1 (C.Sqrt _) e) = do
  e' <- expr e
  return $ Op2 Real Pow e' (ConstR 0.5)
expr (C.Op1 (C.Cast _ _) e) = expr e
expr (C.Op1 op e) = do
  e' <- expr e
  return $ Op1 t' op' e'
  where
    (op', t') = trOp1 op

expr (C.Op2 (C.Ne t) e1 e2) = do
  e1' <- expr e1
  e2' <- expr e2
  return $ Op1 Bool Not (Op2 t' Eq e1' e2')
  where
    t' = trType t

expr (C.Op2 op e1 e2) = do
  e1' <- expr e1
  e2' <- expr e2
  return $ Op2 t' op' e1' e2'
  where
    (op', t') = trOp2 op

expr (C.Op3 (C.Mux t) cond e1 e2) = do
  cond' <- expr cond
  e1'   <- expr e1
  e2'   <- expr e2
  newMux cond' (trType t) e1' e2'
expr (C.Op3 (C.UpdateArray _) _ _ _) = error "There is bug in the type checker"

trConst :: C.Type a -> a -> Expr
trConst t v = case t of
  C.Bool   -> ConstB v
  C.Float  -> negifyR (float2Double v)
  C.Double -> negifyR v
  t'@C.Int8   -> negifyI v (trType t')
  t'@C.Int16  -> negifyI v (trType t')
  t'@C.Int32  -> negifyI v (trType t')
  t'@C.Int64  -> negifyI v (trType t')
  t'@C.Word8  -> negifyI v (trType t')
  t'@C.Word16 -> negifyI v (trType t')
  t'@C.Word32 -> negifyI v (trType t')
  t'@C.Word64 -> negifyI v (trType t')
  t'          -> error $ "There is bug in the type checker" ++ show t'
  where
    negifyR :: Double -> Expr
    negifyR v'
      | v' >= 0   = ConstR v'
      | otherwise = Op1 Real Neg $ ConstR $ negate v'
    negifyI :: Integral a => a -> Type -> Expr
    negifyI v' t'
      | v' >= 0   = ConstI t' $ toInteger v'
      | otherwise = Op1 t' Neg $ ConstI t' $ negate $ toInteger v'

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
  _ -> error "Unsupported unary operator in input."

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

  C.Eq _         -> (Eq, Bool)
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

  _ -> error "Unsupported binary operator in input."

trType :: C.Type a -> Type
trType = \case
  C.Bool   -> Bool
  C.Int8   -> SBV8
  C.Int16  -> SBV16
  C.Int32  -> SBV32
  C.Int64  -> SBV64
  C.Word8  -> BV8
  C.Word16 -> BV16
  C.Word32 -> BV32
  C.Word64 -> BV64
  C.Float  -> Real
  C.Double -> Real
  _        -> error "THere is a bug in the type checker"

-- | Translation state.
data TransST = TransST
  { localConstraints :: [Expr]
  , muxes            :: [(Expr, (Expr, Type, Expr, Expr))]
  , nextFresh        :: Integer
  , addBounds        :: Bool
  }

newMux :: Expr -> Type -> Expr -> Expr -> Trans Expr
newMux c t e1 e2 = do
  ms <- muxes <$> get
  case find ((==mux) . snd) ms of
    Nothing -> do
      f <- fresh
      let v = SVal t (ncMux f) _n_
      modify $ \st -> st { muxes = (v, mux) : ms }
      return v
    Just (v, _) -> return v
  where
    mux = (c, t, e1, e2)

getMuxes :: Trans [Expr]
getMuxes = muxes <$> get >>= return . concat . (map toConstraints)
  where
    toConstraints (v, (c, _, e1, e2)) =
      [ Op2 Bool Or (Op1 Bool Not c) (Op2 Bool Eq v e1)
      , Op2 Bool Or c (Op2 Bool Eq v e2)
      ]

-- | A state monad over the translation state ('TransST').
type Trans = State TransST

fresh :: Trans Integer
fresh = do
  modify $ \st -> st {nextFresh = nextFresh st + 1}
  nextFresh <$> get

localConstraint :: Expr -> Trans ()
localConstraint c =
  modify $ \st -> st {localConstraints = c : localConstraints st}

popLocalConstraints :: Trans [Expr]
popLocalConstraints = liftM2 (++) (localConstraints <$> get) getMuxes
  <* (modify $ \st -> st {localConstraints = [], muxes = []})

runTrans :: Bool -> Trans a -> a
runTrans b m = evalState m $ TransST [] [] 0 b
