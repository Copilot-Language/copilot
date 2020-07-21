{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Copilot.Theorem.What4 where

import qualified Copilot.Core.Expr       as CE
import qualified Copilot.Core.Type       as CT
import qualified Copilot.Core.Type.Array as CT

import qualified What4.Expr.Builder as WB
import qualified What4.Interface    as WI
import qualified What4.BaseTypes    as WT

import Control.Monad (forM)
import Data.Int
import Data.Word
import qualified Data.BitVector.Sized as BV
import Data.Parameterized.Classes
import Data.Parameterized.NatRepr
import Data.Parameterized.Some
import qualified Data.Parameterized.Vector as V

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

translateConstExpr :: forall a t st fs . WB.ExprBuilder t st fs -> CT.Type a -> a -> IO (XExpr t)
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
