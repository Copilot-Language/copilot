--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

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
import Copilot.Core.Type.Equality
import Data.Bits

--------------------------------------------------------------------------------

mkInst :: Equal a b -> f b -> f a
mkInst p con = coerce2 (symm p) con

--------------------------------------------------------------------------------

data ExprInst a = A.Expr a => ExprInst

exprInst :: C.Type a -> ExprInst a
exprInst t =
  case t of
    C.Bool   p -> mkInst p ExprInst
    C.Int8   p -> mkInst p ExprInst ; C.Int16  p -> mkInst p ExprInst
    C.Int32  p -> mkInst p ExprInst ; C.Int64  p -> mkInst p ExprInst
    C.Word8  p -> mkInst p ExprInst ; C.Word16 p -> mkInst p ExprInst
    C.Word32 p -> mkInst p ExprInst ; C.Word64 p -> mkInst p ExprInst
    C.Float  p -> mkInst p ExprInst ; C.Double p -> mkInst p ExprInst

--------------------------------------------------------------------------------

data AssignInst a = A.Assign a => AssignInst

assignInst :: C.Type a -> AssignInst a
assignInst t =
  case t of
    C.Bool   p -> mkInst p AssignInst
    C.Int8   p -> mkInst p AssignInst ; C.Int16  p -> mkInst p AssignInst
    C.Int32  p -> mkInst p AssignInst ; C.Int64  p -> mkInst p AssignInst
    C.Word8  p -> mkInst p AssignInst ; C.Word16 p -> mkInst p AssignInst
    C.Word32 p -> mkInst p AssignInst ; C.Word64 p -> mkInst p AssignInst
    C.Float  p -> mkInst p AssignInst ; C.Double p -> mkInst p AssignInst

--------------------------------------------------------------------------------

data EqEInst a = A.EqE a => EqEInst

eqEInst :: Eq a => C.Type a -> EqEInst a
eqEInst t =
  case t of
    C.Bool   p -> mkInst p EqEInst
    C.Int8   p -> mkInst p EqEInst ; C.Int16  p -> mkInst p EqEInst
    C.Int32  p -> mkInst p EqEInst ; C.Int64  p -> mkInst p EqEInst
    C.Word8  p -> mkInst p EqEInst ; C.Word16 p -> mkInst p EqEInst
    C.Word32 p -> mkInst p EqEInst ; C.Word64 p -> mkInst p EqEInst
    C.Float  p -> mkInst p EqEInst ; C.Double p -> mkInst p EqEInst

--------------------------------------------------------------------------------

data OrdEInst a = A.OrdE a => OrdEInst

ordEInst :: Ord a => C.Type a -> OrdEInst a
ordEInst t =
  case t of
    C.Bool   _ -> error "ordEInst!"
    C.Int8   p -> mkInst p OrdEInst ; C.Int16  p -> mkInst p OrdEInst
    C.Int32  p -> mkInst p OrdEInst ; C.Int64  p -> mkInst p OrdEInst
    C.Word8  p -> mkInst p OrdEInst ; C.Word16 p -> mkInst p OrdEInst
    C.Word32 p -> mkInst p OrdEInst ; C.Word64 p -> mkInst p OrdEInst
    C.Float  p -> mkInst p OrdEInst ; C.Double p -> mkInst p OrdEInst

--------------------------------------------------------------------------------

data NumEInst a = A.NumE a => NumEInst

numEInst :: Num a => C.Type a -> NumEInst a
numEInst t =
  case t of
    C.Bool   _ -> error "numEInst!" -- !! supress warning !!
    C.Int8   p -> mkInst p NumEInst ; C.Int16  p -> mkInst p NumEInst
    C.Int32  p -> mkInst p NumEInst ; C.Int64  p -> mkInst p NumEInst
    C.Word8  p -> mkInst p NumEInst ; C.Word16 p -> mkInst p NumEInst
    C.Word32 p -> mkInst p NumEInst ; C.Word64 p -> mkInst p NumEInst
    C.Float  p -> mkInst p NumEInst ; C.Double p -> mkInst p NumEInst

--------------------------------------------------------------------------------

data IntegralEInst a = A.IntegralE a => IntegralEInst

integralEInst :: Integral a => C.Type a -> IntegralEInst a
integralEInst t =
  case t of
    C.Bool   _ -> error "integralEInst!" -- !! supress warning !!
    C.Int8   p -> mkInst p IntegralEInst ; C.Int16  p -> mkInst p IntegralEInst
    C.Int32  p -> mkInst p IntegralEInst ; C.Int64  p -> mkInst p IntegralEInst
    C.Word8  p -> mkInst p IntegralEInst ; C.Word16 p -> mkInst p IntegralEInst
    C.Word32 p -> mkInst p IntegralEInst ; C.Word64 p -> mkInst p IntegralEInst
    C.Float  _ -> error "integralEInst!" -- !! supress warning !!
    C.Double _ -> error "integralEInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data FloatingEInst a = A.FloatingE a => FloatingEInst

floatingEInst :: Floating a => C.Type a -> FloatingEInst a
floatingEInst t =
  case t of
    C.Float  p -> mkInst p FloatingEInst
    C.Double p -> mkInst p FloatingEInst
    _          -> error "integralEInst!" -- !! supress warning !!

--------------------------------------------------------------------------------

data BitsEInst a = ( A.Expr a, A.OrdE a, A.EqE a, A.IntegralE a, Bits a ) => BitsEInst

bitsEInst :: Bits a => C.Type a -> BitsEInst a
bitsEInst t =
  case t of
    C.Bool   p -> error "bitsEInst Bool!" -- !! supress warning !!
    C.Int8   p -> mkInst p BitsEInst
    C.Int16  p -> mkInst p BitsEInst
    C.Int32  p -> mkInst p BitsEInst
    C.Int64  p -> mkInst p BitsEInst
    C.Word8  p -> mkInst p BitsEInst
    C.Word16 p -> mkInst p BitsEInst
    C.Word32 p -> mkInst p BitsEInst
    C.Word64 p -> mkInst p BitsEInst
    C.Float  _ -> error "bitsEInst Float!" -- !! supress warning !!
    C.Double _ -> error "bitsEInst Double!" -- !! supress warning !!

--------------------------------------------------------------------------------
