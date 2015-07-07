--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Copilot.Compile.C99.Witness
  ( ExprInst (..)      , exprInst
  , AssignInst (..)    , assignInst
  , EqEInst (..)       , eqEInst
  , OrdEInst (..)      , ordEInst
  , NumEInst (..)      , numEInst
  , IntegralEInst (..) , integralEInst
  , FloatingEInst (..) , floatingEInst
  , BitsEInst (..)     , bitsEInst
  ) where

import qualified Language.Atom as A
import qualified Copilot.Core as C
import Data.Bits

--------------------------------------------------------------------------------

badInst :: a
badInst = 
  error "Fatal cast in the witnesses of Atom in copilot-sbv.  Are you sure that Atom supports the type you are using?  If you you are, or you don't understand the error, email leepike @ gmail . com (remove spaces) or file a bug report on github.com"

--------------------------------------------------------------------------------

data ExprInst a = A.Expr a => ExprInst

exprInst :: C.Type a -> ExprInst a
exprInst t =
  case t of
    C.Bool   -> ExprInst
    C.Int8   -> ExprInst ; C.Int16  -> ExprInst
    C.Int32  -> ExprInst ; C.Int64  -> ExprInst
    C.Word8  -> ExprInst ; C.Word16 -> ExprInst
    C.Word32 -> ExprInst ; C.Word64 -> ExprInst
    C.Float  -> ExprInst ; C.Double -> ExprInst
    C.Struct -> badInst

--------------------------------------------------------------------------------

data AssignInst a = A.Assign a => AssignInst

assignInst :: C.Type a -> AssignInst a
assignInst t =
  case t of
    C.Bool   -> AssignInst
    C.Int8   -> AssignInst ; C.Int16  -> AssignInst
    C.Int32  -> AssignInst ; C.Int64  -> AssignInst
    C.Word8  -> AssignInst ; C.Word16 -> AssignInst
    C.Word32 -> AssignInst ; C.Word64 -> AssignInst
    C.Float  -> AssignInst ; C.Double -> AssignInst
    C.Struct -> badInst

--------------------------------------------------------------------------------

data EqEInst a = A.EqE a => EqEInst

eqEInst :: Eq a => C.Type a -> EqEInst a
eqEInst t =
  case t of
    C.Bool   -> EqEInst
    C.Int8   -> EqEInst ; C.Int16  -> EqEInst
    C.Int32  -> EqEInst ; C.Int64  -> EqEInst
    C.Word8  -> EqEInst ; C.Word16 -> EqEInst
    C.Word32 -> EqEInst ; C.Word64 -> EqEInst
    C.Float  -> EqEInst ; C.Double -> EqEInst
    C.Struct -> badInst

--------------------------------------------------------------------------------

data OrdEInst a = A.OrdE a => OrdEInst

ordEInst :: Ord a => C.Type a -> OrdEInst a
ordEInst t =
  case t of
    C.Bool   -> badInst 
    C.Int8   -> OrdEInst ; C.Int16  -> OrdEInst
    C.Int32  -> OrdEInst ; C.Int64  -> OrdEInst
    C.Word8  -> OrdEInst ; C.Word16 -> OrdEInst
    C.Word32 -> OrdEInst ; C.Word64 -> OrdEInst
    C.Float  -> OrdEInst ; C.Double -> OrdEInst
    C.Struct -> badInst

--------------------------------------------------------------------------------

data NumEInst a = A.NumE a => NumEInst

numEInst :: Num a => C.Type a -> NumEInst a
numEInst t =
  case t of
    C.Bool   -> badInst
    C.Int8   -> NumEInst ; C.Int16  -> NumEInst
    C.Int32  -> NumEInst ; C.Int64  -> NumEInst
    C.Word8  -> NumEInst ; C.Word16 -> NumEInst
    C.Word32 -> NumEInst ; C.Word64 -> NumEInst
    C.Float  -> NumEInst ; C.Double -> NumEInst
    C.Struct -> badInst

--------------------------------------------------------------------------------

data IntegralEInst a = A.IntegralE a => IntegralEInst

integralEInst :: Integral a => C.Type a -> IntegralEInst a
integralEInst t =
  case t of
    C.Bool   -> badInst 
    C.Int8   -> IntegralEInst ; C.Int16  -> IntegralEInst
    C.Int32  -> IntegralEInst ; C.Int64  -> IntegralEInst
    C.Word8  -> IntegralEInst ; C.Word16 -> IntegralEInst
    C.Word32 -> IntegralEInst ; C.Word64 -> IntegralEInst
    C.Float  -> badInst
    C.Double -> badInst 
    C.Struct -> badInst

--------------------------------------------------------------------------------

data FloatingEInst a = A.FloatingE a => FloatingEInst

floatingEInst :: Floating a => C.Type a -> FloatingEInst a
floatingEInst t =
  case t of
    C.Float  -> FloatingEInst
    C.Double -> FloatingEInst
    _          -> badInst

--------------------------------------------------------------------------------

data BitsEInst a = ( A.Expr a, A.OrdE a, A.EqE a, A.IntegralE a, Bits a ) => BitsEInst

bitsEInst :: Bits a => C.Type a -> BitsEInst a
bitsEInst t =
  case t of
    C.Bool   -> badInst
    C.Int8   -> BitsEInst
    C.Int16  -> BitsEInst
    C.Int32  -> BitsEInst
    C.Int64  -> BitsEInst
    C.Word8  -> BitsEInst
    C.Word16 -> BitsEInst
    C.Word32 -> BitsEInst
    C.Word64 -> BitsEInst
    C.Float  -> badInst
    C.Double -> badInst
    C.Struct -> badInst

--------------------------------------------------------------------------------
