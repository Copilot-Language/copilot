{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Copilot.Theorem.What4
-- Description : Prove spec properties using What4.
-- Copyright   : (c) Ben Selfridge, 2020
-- Maintainer  : benselfridge@galois.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Spec properties are translated into the language of SMT solvers using
-- @What4@. A backend solver is then used to prove the property is true. The
-- technique is sound, but incomplete. If a property is proved true by this
-- technique, then it can be guaranteed to be true. However, if a property is
-- not proved true, that does not mean it isn't true. Very simple specifications
-- are unprovable by this technique, including:
--
-- @
-- a = True : a
-- @
--
-- The above specification will not be proved true. The reason is that this
-- technique does not perform any sort of induction. When proving the inner @a@
-- expression, the technique merely allocates a fresh constant standing for
-- "@a@, one timestep in the past." Nothing is asserted about the fresh
-- constant.
--
-- An example of a property that is provable by this approach is:
--
-- @
-- a = True : b
-- b = not a
--
-- -- Property: a || b
-- @
--
-- By allocating a fresh constant, @b_-1@, standing for "the value of @b@ one
-- timestep in the past", the equation for @a || b@ at some arbitrary point in
-- the future reduces to @b_-1 || not b_-1@, which is always true.

module Copilot.Theorem.What4
  ( prove, SatResult(..)
  ) where

import qualified Copilot.Core.Expr       as CE
import qualified Copilot.Core.Operators  as CE
import qualified Copilot.Core.Spec       as CS
import qualified Copilot.Core.Type       as CT
import qualified Copilot.Core.Type.Array as CT

import qualified What4.Config           as WC
import qualified What4.Expr.Builder     as WB
import qualified What4.Interface        as WI
import qualified What4.BaseTypes        as WT
import qualified What4.Protocol.SMTLib2 as WS
import qualified What4.Solver           as WS

import Control.Monad.State
import qualified Data.BitVector.Sized as BV
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Parameterized.Classes
import Data.Parameterized.Context
import Data.Parameterized.NatRepr
import Data.Parameterized.Nonce
import Data.Parameterized.Some
import qualified Data.Parameterized.Vector as V
import GHC.TypeNats (KnownNat)

fpRM :: WI.RoundingMode
fpRM = WI.RNE

data BuilderState a = EmptyState

data SatResult = Valid | Invalid | Unknown

data TransState t = TransState {
  -- | Map of all external variables we encounter during translation. These are
  -- just fresh constants. The offset indicates how many timesteps in the past
  -- this constant represents for that stream.
  externVars :: Map.Map (CE.Name, Int) (XExpr t),
  -- | Map from (stream id, negative offset) to fresh constant. These are all
  -- constants representing the values of a stream at some point in the past.
  -- The offset (ALWAYS NEGATIVE) indicates how many timesteps in the past
  -- this constant represents for that stream.
  streamConstants :: Map.Map (CE.Id, Int) (XExpr t),
  -- | Map from stream ids to the streams themselves. This value is never
  -- modified, but I didn't want to make this an RWS, so it's represented as a
  -- stateful value.
  streams :: Map.Map CE.Id CS.Stream,
  -- | Binary power operator, represented as an uninterpreted function.
  pow :: WB.ExprSymFn t (WB.Expr t)
         (EmptyCtx ::> WT.BaseRealType ::> WT.BaseRealType)
         WT.BaseRealType,
  -- | Binary logarithm operator, represented as an uninterpreted function.
  logb :: WB.ExprSymFn t (WB.Expr t)
          (EmptyCtx ::> WT.BaseRealType ::> WT.BaseRealType)
          WT.BaseRealType
  }

newtype TransM t a = TransM { unTransM :: StateT (TransState t) IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadState (TransState t)
           )

instance MonadFail (TransM t) where
  fail = error

prove :: FilePath
      -- ^ Path to z3 executable
      -> CS.Spec
      -- ^ Spec
      -> IO [(CE.Name, SatResult)]
prove z3path spec = do
  Some ng <- newIONonceGenerator
  sym <- WB.newExprBuilder WB.FloatIEEERepr EmptyState ng
  WC.extendConfig WS.z3Options (WI.getConfiguration sym)
  let streamMap = Map.fromList $
        (\stream -> (CS.streamId stream, stream)) <$> CS.specStreams spec
  pow <- WI.freshTotalUninterpFn sym (WI.safeSymbol "pow") knownRepr knownRepr
  logb <- WI.freshTotalUninterpFn sym (WI.safeSymbol "logb") knownRepr knownRepr
  let st = TransState Map.empty Map.empty streamMap pow logb
  -- TODO: This should be done within TransM
  forM (CS.specProperties spec) $ \pr -> do
    (XBool p, _) <- runStateT (unTransM $ translateExpr sym 0 (CS.propertyExpr pr)) st
    not_p <- WI.notPred sym p
    WS.withZ3 sym z3path WS.defaultLogData $ \session -> do
      WS.assume (WS.sessionWriter session) not_p
      fmap (CS.propertyName pr, ) $ WS.runCheckSat session $ \case
        WS.Sat (_ge, _) -> return Invalid
        WS.Unsat _ -> return Valid
        WS.Unknown -> return Unknown

-- | The What4 representation of a copilot expression.
data XExpr t where
  XBool :: WB.Expr t WT.BaseBoolType -> XExpr t
  XInt8 :: WB.Expr t (WT.BaseBVType 8) -> XExpr t
  XInt16 :: WB.Expr t (WT.BaseBVType 16) -> XExpr t
  XInt32 :: WB.Expr t (WT.BaseBVType 32) -> XExpr t
  XInt64 :: WB.Expr t (WT.BaseBVType 64) -> XExpr t
  XWord8 :: WB.Expr t (WT.BaseBVType 8) -> XExpr t
  XWord16 :: WB.Expr t (WT.BaseBVType 16) -> XExpr t
  XWord32 :: WB.Expr t (WT.BaseBVType 32) -> XExpr t
  XWord64 :: WB.Expr t (WT.BaseBVType 64) -> XExpr t
  XFloat :: WB.Expr t (WT.BaseFloatType WT.Prec32) -> XExpr t
  XDouble :: WB.Expr t (WT.BaseFloatType WT.Prec64) -> XExpr t
  XEmptyArray :: XExpr t
  XArray :: 1 <= n => V.Vector n (XExpr t) -> XExpr t
  XStruct :: [XExpr t] -> XExpr t

deriving instance Show (XExpr t)

data BVSign = Signed | Unsigned

data SomeBVExpr t where
  SomeBVExpr :: 1 <= w
             => WB.BVExpr t w
             -> NatRepr w
             -> BVSign
             -> (WB.BVExpr t w -> XExpr t)
             -> SomeBVExpr t

asBVExpr :: XExpr t -> Maybe (SomeBVExpr t)
asBVExpr xe = case xe of
  XInt8 e -> Just (SomeBVExpr e knownNat Signed XInt8)
  XInt16 e -> Just (SomeBVExpr e knownNat Signed XInt16)
  XInt32 e -> Just (SomeBVExpr e knownNat Signed XInt32)
  XInt64 e -> Just (SomeBVExpr e knownNat Signed XInt64)
  XWord8 e -> Just (SomeBVExpr e knownNat Unsigned XWord8)
  XWord16 e -> Just (SomeBVExpr e knownNat Unsigned XWord16)
  XWord32 e -> Just (SomeBVExpr e knownNat Unsigned XWord32)
  XWord64 e -> Just (SomeBVExpr e knownNat Unsigned XWord64)
  _ -> Nothing

data SomeFPExpr t = forall fpp . SomeFPExpr (WB.Expr t (WT.BaseFloatType fpp))

asFPExpr :: XExpr t -> Maybe (SomeFPExpr t)
asFPExpr xe = case xe of
  XFloat e -> Just (SomeFPExpr e)
  XDouble e -> Just (SomeFPExpr e)
  _ -> Nothing

translateConstExpr :: forall a t st fs .
                      WB.ExprBuilder t st fs
                   -> CT.Type a
                   -> a
                   -> IO (XExpr t)
translateConstExpr sym tp a = case tp of
  CT.Bool -> case a of
    True  -> return $ XBool (WI.truePred sym)
    False -> return $ XBool (WI.falsePred sym)
  CT.Int8 -> XInt8 <$> WI.bvLit sym knownNat (BV.int8 a)
  CT.Int16 -> XInt16 <$> WI.bvLit sym knownNat (BV.int16 a)
  CT.Int32 -> XInt32 <$> WI.bvLit sym knownNat (BV.int32 a)
  CT.Int64 -> XInt64 <$> WI.bvLit sym knownNat (BV.int64 a)
  CT.Word8 -> XWord8 <$> WI.bvLit sym knownNat (BV.word8 a)
  CT.Word16 -> XWord16 <$> WI.bvLit sym knownNat (BV.word16 a)
  CT.Word32 -> XWord32 <$> WI.bvLit sym knownNat (BV.word32 a)
  CT.Word64 -> XWord64 <$> WI.bvLit sym knownNat (BV.word64 a)
  CT.Float -> XFloat <$> WI.floatLit sym knownRepr (toRational a)
  CT.Double -> XDouble <$> WI.floatLit sym knownRepr (toRational a)
  CT.Array tp -> do
    elts <- traverse (translateConstExpr sym tp) (CT.arrayelems a)
    Just (Some n) <- return $ someNat (length elts)
    case isZeroOrGT1 n of
      Left Refl -> return XEmptyArray
      Right LeqProof -> do
        let Just v = V.fromList n elts
        return $ XArray v
  CT.Struct _ -> do
    elts <- forM (CT.toValues a) $ \(CT.Value tp (CT.Field a)) ->
      translateConstExpr sym tp a
    return $ XStruct elts

arrayLen :: KnownNat n => CT.Type (CT.Array n t) -> NatRepr n
arrayLen _ = knownNat

freshCPConstant :: forall t st fs a .
                   WB.ExprBuilder t st fs
                -> String
                -> CT.Type a
                -> IO (XExpr t)
freshCPConstant sym nm tp = case tp of
  CT.Bool -> XBool <$> WI.freshConstant sym (WI.safeSymbol nm) knownRepr
  CT.Int8 -> XInt8 <$> WI.freshConstant sym (WI.safeSymbol nm) knownRepr
  CT.Int16 -> XInt16 <$> WI.freshConstant sym (WI.safeSymbol nm) knownRepr
  CT.Int32 -> XInt32 <$> WI.freshConstant sym (WI.safeSymbol nm) knownRepr
  CT.Int64 -> XInt64 <$> WI.freshConstant sym (WI.safeSymbol nm) knownRepr
  CT.Word8 -> XWord8 <$> WI.freshConstant sym (WI.safeSymbol nm) knownRepr
  CT.Word16 -> XWord16 <$> WI.freshConstant sym (WI.safeSymbol nm) knownRepr
  CT.Word32 -> XWord32 <$> WI.freshConstant sym (WI.safeSymbol nm) knownRepr
  CT.Word64 -> XWord64 <$> WI.freshConstant sym (WI.safeSymbol nm) knownRepr
  CT.Float -> XFloat <$> WI.freshConstant sym (WI.safeSymbol nm) knownRepr
  CT.Double -> XDouble <$> WI.freshConstant sym (WI.safeSymbol nm) knownRepr
  atp@(CT.Array itp) -> do
    n <- return $ arrayLen atp
    case isZeroOrGT1 n of
      Left Refl -> return XEmptyArray
      Right LeqProof -> do
        elts :: V.Vector n (XExpr t) <- V.generateM (decNat n) (const (freshCPConstant sym "" itp))
        Refl <- return $ minusPlusCancel n (knownNat @1)
        return $ XArray elts
  CT.Struct stp -> do
    elts <- forM (CT.toValues stp) $ \(CT.Value ftp _) -> freshCPConstant sym "" ftp
    return $ XStruct elts

-- | Get the constant for a given stream id and some offset into the past. This
-- should only be called with a strictly negative offset. When this function
-- gets called for the first time for a given (streamId, offset) pair, it
-- generates a fresh constant and stores it in an internal map. Thereafter, this
-- function will just return that constant when called with the same pair.
getStreamConstant :: WB.ExprBuilder t st fs -> CE.Id -> Int -> TransM t (XExpr t)
getStreamConstant sym streamId offset = do
  scs <- gets streamConstants
  case Map.lookup (streamId, offset) scs of
    Just xe -> return xe
    Nothing -> do
      CS.Stream _ _ _ tp <- getStreamDef streamId
      let nm = show streamId ++ "_" ++ show offset
      xe <- liftIO $ freshCPConstant sym nm tp
      modify (\st -> st { streamConstants = Map.insert (streamId, offset) xe scs })
      return xe

-- | Get the constant for a given external variable and some offset into the
-- past. This hsould only be called with a strictly negative offset. When this
-- function gets called for the first time for a given (var, offset) pair, it
-- generates a fresh constant and stores it in an internal map. Thereafter, this
-- function will just return that constant when called with the same pair.
getExternConstant :: WB.ExprBuilder t st fs
                  -> CT.Type a
                  -> CE.Name
                  -> Int
                  -> TransM t (XExpr t)
getExternConstant sym tp var offset = do
  es <- gets externVars
  case Map.lookup (var, offset) es of
    Just xe -> return xe
    Nothing -> do
      xe <- liftIO $ freshCPConstant sym var tp
      modify (\st -> st { externVars = Map.insert (var, offset) xe es} )
      return xe

-- | Retrieve a stream definition given its id.
getStreamDef :: CE.Id -> TransM t CS.Stream
getStreamDef streamId = fromJust <$> gets (Map.lookup streamId . streams)

-- | Translate an expression into a what4 representation. The int offset keeps
-- track of how many timesteps into the past each variable is referring to.
-- Initially the value should be zero, but when we translate a stream, the
-- offset is recomputed based on the length of that stream's prefix (subtracted)
-- and the drop index (added).
translateExpr :: WB.ExprBuilder t st fs
              -> Int
              -- ^ number of timesteps in the past we are currently looking
              -- (must always be <= 0)
              -> CE.Expr a
              -> TransM t (XExpr t)
translateExpr sym offset e = case e of
  CE.Const tp a -> liftIO $ translateConstExpr sym tp a
  CE.Drop _tp ix streamId
    -- If we are referencing a past value of this stream, just return an
    -- unconstrained constant.
    | offset + fromIntegral ix < 0 ->
        getStreamConstant sym streamId (offset + fromIntegral ix)
    -- If we are referencing a current or future value of this stream, we need
    -- to translate the stream's expression, using an offset computed based on
    -- the current offset (negative or 0), the drop index (positive or 0), and
    -- the length of the stream's buffer (subtracted).
    | otherwise -> do
      CS.Stream _ buf e _ <- getStreamDef streamId
      translateExpr sym (offset + fromIntegral ix - length buf) e
  CE.Local _ _ _ _ _ -> error "translateExpr: Local unimplemented"
  CE.Var _ _ -> error "translateExpr: Var unimplemented"
  CE.ExternVar tp nm _prefix -> getExternConstant sym tp nm offset
  CE.Op1 op e -> liftIO . translateOp1 sym op =<< translateExpr sym offset e
  CE.Op2 op e1 e2 -> do
    xe1 <- translateExpr sym offset e1
    xe2 <- translateExpr sym offset e2
    powFn <- gets pow
    logbFn <- gets logb
    liftIO $ translateOp2 sym powFn logbFn op xe1 xe2
  CE.Op3 op e1 e2 e3 -> do
    xe1 <- translateExpr sym offset e1
    xe2 <- translateExpr sym offset e2
    xe3 <- translateExpr sym offset e3
    liftIO $ translateOp3 sym op xe1 xe2 xe3
  CE.Label _ _ _ -> error "translateExpr: Label unimplemented"

type BVOp1 w t = (KnownNat w, 1 <= w) => WB.BVExpr t w -> IO (WB.BVExpr t w)

type FPOp1 fpp t = KnownRepr WT.FloatPrecisionRepr fpp => WB.Expr t (WT.BaseFloatType fpp) -> IO (WB.Expr t (WT.BaseFloatType fpp))

type RealOp1 t = WB.Expr t WT.BaseRealType -> IO (WB.Expr t WT.BaseRealType)

translateOp1 :: forall t st fs a b .
                WB.ExprBuilder t st fs
             -> CE.Op1 a b
             -> XExpr t
             -> IO (XExpr t)
translateOp1 sym op xe = case (op, xe) of
  (CE.Not, XBool e) -> XBool <$> WI.notPred sym e
  (CE.Abs _, xe) -> numOp bvAbs fpAbs xe
    where bvAbs :: BVOp1 w t
          bvAbs e = do zero <- WI.bvLit sym knownNat (BV.zero knownNat)
                       e_neg <- WI.bvSlt sym e zero
                       neg_e <- WI.bvSub sym zero e
                       WI.bvIte sym e_neg neg_e e
          fpAbs :: FPOp1 fpp t
          fpAbs e = do zero <- WI.floatLit sym knownRepr 0
                       e_neg <- WI.floatLt sym e zero
                       neg_e <- WI.floatSub sym fpRM zero e
                       WI.floatIte sym e_neg neg_e e
  (CE.Sign _, xe) -> numOp bvSign fpSign xe
    where bvSign :: BVOp1 w t
          bvSign e = do zero <- WI.bvLit sym knownRepr (BV.zero knownNat)
                        neg_one <- WI.bvLit sym knownNat (BV.mkBV knownNat (-1))
                        pos_one <- WI.bvLit sym knownNat (BV.mkBV knownNat 1)
                        e_zero <- WI.bvEq sym e zero
                        e_neg <- WI.bvSlt sym e zero
                        t <- WI.bvIte sym e_neg neg_one pos_one
                        WI.bvIte sym e_zero zero t
          fpSign :: FPOp1 fpp t
          fpSign e = do zero <- WI.floatLit sym knownRepr 0
                        neg_one <- WI.floatLit sym knownRepr (-1)
                        pos_one <- WI.floatLit sym knownRepr 1
                        e_zero <- WI.floatEq sym e zero
                        e_neg <- WI.floatLt sym e zero
                        t <- WI.floatIte sym e_neg neg_one pos_one
                        WI.floatIte sym e_zero zero t
  (CE.Recip _, xe) -> fpOp recip xe
    where recip :: FPOp1 fpp t
          recip e = do one <- WI.floatLit sym knownRepr 1
                       WI.floatDiv sym fpRM one e
  (CE.Exp _, xe) -> realOp (WI.realExp sym) xe
  (CE.Sqrt _, xe) -> fpOp (WI.floatSqrt sym fpRM) xe
  (CE.Log _, xe) -> realOp (WI.realLog sym) xe
  (CE.Sin _, xe) -> realOp (WI.realSin sym) xe
  (CE.Cos _, xe) -> realOp (WI.realCos sym) xe
  (CE.Tan _, xe) -> realOp (WI.realTan sym) xe
  (CE.Asin _, xe) -> realOp (realRecip <=< WI.realSin sym) xe
  (CE.Acos _, xe) -> realOp (realRecip <=< WI.realCos sym) xe
  (CE.Atan _, xe) -> realOp (realRecip <=< WI.realTan sym) xe
  (CE.Sinh _, xe) -> realOp (WI.realSinh sym) xe
  (CE.Cosh _, xe) -> realOp (WI.realCosh sym) xe
  (CE.Tanh _, xe) -> realOp (WI.realTanh sym) xe
  (CE.Asinh _, xe) -> realOp (realRecip <=< WI.realSinh sym) xe
  (CE.Acosh _, xe) -> realOp (realRecip <=< WI.realCosh sym) xe
  (CE.Atanh _, xe) -> realOp (realRecip <=< WI.realTanh sym) xe
  (CE.BwNot _, xe) -> case xe of
    XBool e -> XBool <$> WI.notPred sym e
    _ -> bvOp (WI.bvNotBits sym) xe
  (CE.Cast _ tp, xe) -> case (xe, tp) of
    (XBool e, CT.Bool) -> return $ XBool e
    (XBool e, CT.Word8) -> XWord8 <$> WI.predToBV sym e knownNat
    (XBool e, CT.Word16) -> XWord16 <$> WI.predToBV sym e knownNat
    (XBool e, CT.Word32) -> XWord32 <$> WI.predToBV sym e knownNat
    (XBool e, CT.Word64) -> XWord64 <$> WI.predToBV sym e knownNat
    (XBool e, CT.Int8) -> XInt8 <$> WI.predToBV sym e knownNat
    (XBool e, CT.Int16) -> XInt16 <$> WI.predToBV sym e knownNat
    (XBool e, CT.Int32) -> XInt32 <$> WI.predToBV sym e knownNat
    (XBool e, CT.Int64) -> XInt64 <$> WI.predToBV sym e knownNat
    (XInt8 e, CT.Int8) -> return $ XInt8 e
    (XInt8 e, CT.Int16) -> XInt16 <$> WI.bvSext sym knownNat e
    (XInt8 e, CT.Int32) -> XInt32 <$> WI.bvSext sym knownNat e
    (XInt8 e, CT.Int64) -> XInt64 <$> WI.bvSext sym knownNat e
    (XInt16 e, CT.Int16) -> return $ XInt16 e
    (XInt16 e, CT.Int32) -> XInt32 <$> WI.bvSext sym knownNat e
    (XInt16 e, CT.Int64) -> XInt64 <$> WI.bvSext sym knownNat e
    (XInt32 e, CT.Int32) -> return $ XInt32 e
    (XInt32 e, CT.Int64) -> XInt64 <$> WI.bvSext sym knownNat e
    (XInt64 e, CT.Int64) -> return $ XInt64 e
    (XWord8 e, CT.Int16) -> XInt16 <$> WI.bvZext sym knownNat e
    (XWord8 e, CT.Int32) -> XInt32 <$> WI.bvZext sym knownNat e
    (XWord8 e, CT.Int64) -> XInt64 <$> WI.bvZext sym knownNat e
    (XWord8 e, CT.Word8) -> return $ XWord8 e
    (XWord8 e, CT.Word16) -> XWord16 <$> WI.bvZext sym knownNat e
    (XWord8 e, CT.Word32) -> XWord32 <$> WI.bvZext sym knownNat e
    (XWord8 e, CT.Word64) -> XWord64 <$> WI.bvZext sym knownNat e
    (XWord16 e, CT.Int32) -> XInt32 <$> WI.bvZext sym knownNat e
    (XWord16 e, CT.Int64) -> XInt64 <$> WI.bvZext sym knownNat e
    (XWord16 e, CT.Word16) -> return $ XWord16 e
    (XWord16 e, CT.Word32) -> XWord32 <$> WI.bvZext sym knownNat e
    (XWord16 e, CT.Word64) -> XWord64 <$> WI.bvZext sym knownNat e
    (XWord32 e, CT.Int64) -> XInt64 <$> WI.bvZext sym knownNat e
    (XWord32 e, CT.Word32) -> return $ XWord32 e
    (XWord32 e, CT.Word64) -> XWord64 <$> WI.bvZext sym knownNat e
    (XWord64 e, CT.Word64) -> return $ XWord64 e
  where numOp :: (forall w . BVOp1 w t)
              -> (forall fpp . FPOp1 fpp t)
              -> XExpr t
              -> IO (XExpr t)
        numOp bvOp fpOp xe = case xe of
          XInt8 e -> XInt8 <$> bvOp e
          XInt16 e -> XInt16 <$> bvOp e
          XInt32 e -> XInt32 <$> bvOp e
          XInt64 e -> XInt64 <$> bvOp e
          XWord8 e -> XWord8 <$> bvOp e
          XWord16 e -> XWord16 <$> bvOp e
          XWord32 e -> XWord32 <$> bvOp e
          XWord64 e -> XWord64 <$> bvOp e
          XFloat e -> XFloat <$> fpOp e
          XDouble e -> XDouble <$> fpOp e

        bvOp :: (forall w . BVOp1 w t) -> XExpr t -> IO (XExpr t)
        bvOp f xe = case xe of
          XInt8 e -> XInt8 <$> f e
          XInt16 e -> XInt16 <$> f e
          XInt32 e -> XInt32 <$> f e
          XInt64 e -> XInt64 <$> f e
          XWord8 e -> XWord8 <$> f e
          XWord16 e -> XWord16 <$> f e
          XWord32 e -> XWord32 <$> f e
          XWord64 e -> XWord64 <$> f e

        fpOp :: (forall fpp . FPOp1 fpp t) -> XExpr t -> IO (XExpr t)
        fpOp g xe = case xe of
          XFloat e -> XFloat <$> g e
          XDouble e -> XDouble <$> g e

        realOp :: RealOp1 t -> XExpr t -> IO (XExpr t)
        realOp h xe = fpOp hf xe
          where hf :: (forall fpp . FPOp1 fpp t)
                hf e = do re <- WI.floatToReal sym e
                          hre <- h re
                          WI.realToFloat sym knownRepr fpRM hre

        realRecip :: RealOp1 t
        realRecip e = do one <- WI.realLit sym 1
                         WI.realDiv sym one e

type BVOp2 w t = (KnownNat w, 1 <= w) => WB.BVExpr t w -> WB.BVExpr t w -> IO (WB.BVExpr t w)

type FPOp2 fpp t = KnownRepr WT.FloatPrecisionRepr fpp => WB.Expr t (WT.BaseFloatType fpp) -> WB.Expr t (WT.BaseFloatType fpp) -> IO (WB.Expr t (WT.BaseFloatType fpp))

type RealOp2 t = WB.Expr t WT.BaseRealType -> WB.Expr t WT.BaseRealType -> IO (WB.Expr t WT.BaseRealType)

type BoolCmp2 t = WB.BoolExpr t -> WB.BoolExpr t -> IO (WB.BoolExpr t)

type BVCmp2 w t = (KnownNat w, 1 <= w) => WB.BVExpr t w -> WB.BVExpr t w -> IO (WB.BoolExpr t)

type FPCmp2 fpp t = KnownRepr WT.FloatPrecisionRepr fpp => WB.Expr t (WT.BaseFloatType fpp) -> WB.Expr t (WT.BaseFloatType fpp) -> IO (WB.BoolExpr t)

translateOp2 :: forall t st fs a b c .
                WB.ExprBuilder t st fs
             -> (WB.ExprSymFn t (WB.Expr t)
                 (EmptyCtx ::> WT.BaseRealType ::> WT.BaseRealType)
                 WT.BaseRealType)
             -> (WB.ExprSymFn t (WB.Expr t)
                 (EmptyCtx ::> WT.BaseRealType ::> WT.BaseRealType)
                 WT.BaseRealType)
             -> CE.Op2 a b c
             -> XExpr t
             -> XExpr t
             -> IO (XExpr t)
translateOp2 sym powFn logbFn op xe1 xe2 = case (op, xe1, xe2) of
  (CE.And, XBool e1, XBool e2) -> XBool <$> WI.andPred sym e1 e2
  (CE.Or, XBool e1, XBool e2) -> XBool <$> WI.orPred sym e1 e2
  (CE.Add _, xe1, xe2) -> numOp (WI.bvAdd sym) (WI.floatAdd sym fpRM) xe1 xe2
  (CE.Sub _, xe1, xe2) -> numOp (WI.bvSub sym) (WI.floatSub sym fpRM) xe1 xe2
  (CE.Mul _, xe1, xe2) -> numOp (WI.bvMul sym) (WI.floatMul sym fpRM) xe1 xe2
  (CE.Mod _, xe1, xe2) -> bvOp (WI.bvSrem sym) (WI.bvUrem sym) xe1 xe2
  (CE.Div _, xe1, xe2) -> bvOp (WI.bvSdiv sym) (WI.bvUdiv sym) xe1 xe2
  (CE.Fdiv _, xe1, xe2) -> fpOp (WI.floatDiv sym fpRM) xe1 xe2
  (CE.Pow _, xe1, xe2) -> fpOp powFn' xe1 xe2
    where powFn' :: FPOp2 fpp t
          powFn' e1 e2 = do re1 <- WI.floatToReal sym e1
                            re2 <- WI.floatToReal sym e2
                            let args = (Empty :> re1 :> re2)
                            rpow <- WI.applySymFn sym powFn args
                            WI.realToFloat sym knownRepr fpRM rpow
  (CE.Logb _, xe1, xe2) -> fpOp logbFn' xe1 xe2
    where logbFn' :: FPOp2 fpp t
          logbFn' e1 e2 = do re1 <- WI.floatToReal sym e1
                             re2 <- WI.floatToReal sym e2
                             let args = (Empty :> re1 :> re2)
                             rpow <- WI.applySymFn sym logbFn args
                             WI.realToFloat sym knownRepr fpRM rpow
  (CE.Eq _, xe1, xe2) -> cmp (WI.eqPred sym) (WI.bvEq sym) (WI.floatEq sym) xe1 xe2
  (CE.Ne _, xe1, xe2) -> cmp neqPred bvNeq fpNeq xe1 xe2
    where neqPred :: BoolCmp2 t
          neqPred e1 e2 = do e <- WI.eqPred sym e1 e2
                             WI.notPred sym e
          bvNeq :: forall w . BVCmp2 w t
          bvNeq e1 e2 = do e <- WI.bvEq sym e1 e2
                           WI.notPred sym e
          fpNeq :: forall fpp . FPCmp2 fpp t
          fpNeq e1 e2 = do e <- WI.floatEq sym e1 e2
                           WI.notPred sym e
  (CE.Le _, xe1, xe2) -> numCmp (WI.bvSle sym) (WI.bvUle sym) (WI.floatLe sym) xe1 xe2
  (CE.Ge _, xe1, xe2) -> numCmp (WI.bvSge sym) (WI.bvUge sym) (WI.floatGe sym) xe1 xe2
  (CE.Lt _, xe1, xe2) -> numCmp (WI.bvSlt sym) (WI.bvUlt sym) (WI.floatLt sym) xe1 xe2
  (CE.Gt _, xe1, xe2) -> numCmp (WI.bvSgt sym) (WI.bvUgt sym) (WI.floatGt sym) xe1 xe2
  (CE.BwAnd _, xe1, xe2) -> bvOp (WI.bvAndBits sym) (WI.bvAndBits sym) xe1 xe2
  (CE.BwOr _, xe1, xe2) -> bvOp (WI.bvOrBits sym) (WI.bvOrBits sym) xe1 xe2
  (CE.BwXor _, xe1, xe2) -> bvOp (WI.bvXorBits sym) (WI.bvXorBits sym) xe1 xe2
  -- Note: For both shift operators, we are interpreting the shifter as an
  -- unsigned bitvector regardless of whether it is a word or an int.
  (CE.BwShiftL _ _, xe1, xe2) -> do
    Just (SomeBVExpr e1 w1 _ ctor1) <- return $ asBVExpr xe1
    Just (SomeBVExpr e2 w2 _ _    ) <- return $ asBVExpr xe2
    e2' <- case testNatCases w1 w2 of
      NatCaseLT LeqProof -> WI.bvTrunc sym w1 e2
      NatCaseEQ -> return e2
      NatCaseGT LeqProof -> WI.bvZext sym w1 e2
    ctor1 <$> WI.bvShl sym e1 e2'
  (CE.BwShiftR _ _, xe1, xe2) -> do
    Just (SomeBVExpr e1 w1 sgn1 ctor1) <- return $ asBVExpr xe1
    Just (SomeBVExpr e2 w2 _    _    ) <- return $ asBVExpr xe2
    e2' <- case testNatCases w1 w2 of
      NatCaseLT LeqProof -> WI.bvTrunc sym w1 e2
      NatCaseEQ -> return e2
      NatCaseGT LeqProof -> WI.bvZext sym w1 e2
    ctor1 <$> case sgn1 of
      Signed -> WI.bvAshr sym e1 e2'
      Unsigned -> WI.bvLshr sym e1 e2'
  _ -> undefined
  where numOp :: (forall w . BVOp2 w t)
              -> (forall fpp . FPOp2 fpp t)
              -> XExpr t
              -> XExpr t
              -> IO (XExpr t)
        numOp bvOp fpOp xe1 xe2 = case (xe1, xe2) of
          (XInt8 e1, XInt8 e2) -> XInt8 <$> bvOp e1 e2
          (XInt16 e1, XInt16 e2) -> XInt16 <$> bvOp e1 e2
          (XInt32 e1, XInt32 e2)-> XInt32 <$> bvOp e1 e2
          (XInt64 e1, XInt64 e2)-> XInt64 <$> bvOp e1 e2
          (XWord8 e1, XWord8 e2)-> XWord8 <$> bvOp e1 e2
          (XWord16 e1, XWord16 e2)-> XWord16 <$> bvOp e1 e2
          (XWord32 e1, XWord32 e2)-> XWord32 <$> bvOp e1 e2
          (XWord64 e1, XWord64 e2)-> XWord64 <$> bvOp e1 e2
          (XFloat e1, XFloat e2)-> XFloat <$> fpOp e1 e2
          (XDouble e1, XDouble e2)-> XDouble <$> fpOp e1 e2

        bvOp :: (forall w . BVOp2 w t)
             -> (forall w . BVOp2 w t)
             -> XExpr t
             -> XExpr t
             -> IO (XExpr t)
        bvOp opS opU xe1 xe2 = case (xe1, xe2) of
          (XInt8 e1, XInt8 e2) -> XInt8 <$> opS e1 e2
          (XInt16 e1, XInt16 e2) -> XInt16 <$> opS e1 e2
          (XInt32 e1, XInt32 e2) -> XInt32 <$> opS e1 e2
          (XInt64 e1, XInt64 e2) -> XInt64 <$> opS e1 e2
          (XWord8 e1, XWord8 e2) -> XWord8 <$> opU e1 e2
          (XWord16 e1, XWord16 e2) -> XWord16 <$> opU e1 e2
          (XWord32 e1, XWord32 e2) -> XWord32 <$> opU e1 e2
          (XWord64 e1, XWord64 e2) -> XWord64 <$> opU e1 e2

        fpOp :: (forall fpp . FPOp2 fpp t)
             -> XExpr t
             -> XExpr t
             -> IO (XExpr t)
        fpOp op xe1 xe2 = case (xe1, xe2) of
          (XFloat e1, XFloat e2) -> XFloat <$> op e1 e2
          (XDouble e1, XDouble e2) -> XDouble <$> op e1 e2

        cmp :: BoolCmp2 t
            -> (forall w . BVCmp2 w t)
            -> (forall fpp . FPCmp2 fpp t)
            -> XExpr t
            -> XExpr t
            -> IO (XExpr t)
        cmp boolOp bvOp fpOp xe1 xe2 = case (xe1, xe2) of
          (XBool e1, XBool e2) -> XBool <$> boolOp e1 e2
          (XInt8 e1, XInt8 e2) -> XBool <$> bvOp e1 e2
          (XInt16 e1, XInt16 e2) -> XBool <$> bvOp e1 e2
          (XInt32 e1, XInt32 e2)-> XBool <$> bvOp e1 e2
          (XInt64 e1, XInt64 e2)-> XBool <$> bvOp e1 e2
          (XWord8 e1, XWord8 e2)-> XBool <$> bvOp e1 e2
          (XWord16 e1, XWord16 e2)-> XBool <$> bvOp e1 e2
          (XWord32 e1, XWord32 e2)-> XBool <$> bvOp e1 e2
          (XWord64 e1, XWord64 e2)-> XBool <$> bvOp e1 e2
          (XFloat e1, XFloat e2)-> XBool <$> fpOp e1 e2
          (XDouble e1, XDouble e2)-> XBool <$> fpOp e1 e2

        numCmp :: (forall w . BVCmp2 w t)
               -> (forall w . BVCmp2 w t)
               -> (forall fpp . FPCmp2 fpp t)
               -> XExpr t
               -> XExpr t
               -> IO (XExpr t)
        numCmp bvSOp bvUOp fpOp xe1 xe2 = case (xe1, xe2) of
          (XInt8 e1, XInt8 e2) -> XBool <$> bvSOp e1 e2
          (XInt16 e1, XInt16 e2) -> XBool <$> bvSOp e1 e2
          (XInt32 e1, XInt32 e2)-> XBool <$> bvSOp e1 e2
          (XInt64 e1, XInt64 e2)-> XBool <$> bvSOp e1 e2
          (XWord8 e1, XWord8 e2)-> XBool <$> bvUOp e1 e2
          (XWord16 e1, XWord16 e2)-> XBool <$> bvUOp e1 e2
          (XWord32 e1, XWord32 e2)-> XBool <$> bvUOp e1 e2
          (XWord64 e1, XWord64 e2)-> XBool <$> bvUOp e1 e2
          (XFloat e1, XFloat e2)-> XBool <$> fpOp e1 e2
          (XDouble e1, XDouble e2)-> XBool <$> fpOp e1 e2

translateOp3 :: forall t st fs a b c d . WB.ExprBuilder t st fs -> CE.Op3 a b c d -> XExpr t -> XExpr t -> XExpr t -> IO (XExpr t)
translateOp3 = undefined
