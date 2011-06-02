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
  ) where

import qualified Language.Atom as A
import qualified Copilot.Core as C
import Copilot.Core.Type.Equality

--------------------------------------------------------------------------------

mkInst :: Equal α β -> f β -> f α
mkInst p con = coerce2 (symm p) con

--------------------------------------------------------------------------------

data ExprInst α = A.Expr α => ExprInst

exprInst :: C.Type α -> ExprInst α
exprInst t =
  case t of
    C.Bool   p -> mkInst p ExprInst
    C.Int8   p -> mkInst p ExprInst ; C.Int16  p -> mkInst p ExprInst
    C.Int32  p -> mkInst p ExprInst ; C.Int64  p -> mkInst p ExprInst
    C.Word8  p -> mkInst p ExprInst ; C.Word16 p -> mkInst p ExprInst
    C.Word32 p -> mkInst p ExprInst ; C.Word64 p -> mkInst p ExprInst
    C.Float  p -> mkInst p ExprInst ; C.Double p -> mkInst p ExprInst

--------------------------------------------------------------------------------

data AssignInst α = A.Assign α => AssignInst

assignInst :: C.Type α -> AssignInst α
assignInst t =
  case t of
    C.Bool   p -> mkInst p AssignInst
    C.Int8   p -> mkInst p AssignInst ; C.Int16  p -> mkInst p AssignInst
    C.Int32  p -> mkInst p AssignInst ; C.Int64  p -> mkInst p AssignInst
    C.Word8  p -> mkInst p AssignInst ; C.Word16 p -> mkInst p AssignInst
    C.Word32 p -> mkInst p AssignInst ; C.Word64 p -> mkInst p AssignInst
    C.Float  p -> mkInst p AssignInst ; C.Double p -> mkInst p AssignInst

--------------------------------------------------------------------------------

data EqEInst α = A.EqE α => EqEInst

eqEInst :: Eq α => C.Type α -> EqEInst α
eqEInst t =
  case t of
    C.Bool   p -> mkInst p EqEInst
    C.Int8   p -> mkInst p EqEInst ; C.Int16  p -> mkInst p EqEInst
    C.Int32  p -> mkInst p EqEInst ; C.Int64  p -> mkInst p EqEInst
    C.Word8  p -> mkInst p EqEInst ; C.Word16 p -> mkInst p EqEInst
    C.Word32 p -> mkInst p EqEInst ; C.Word64 p -> mkInst p EqEInst
    C.Float  p -> mkInst p EqEInst ; C.Double p -> mkInst p EqEInst

--------------------------------------------------------------------------------

data OrdEInst α = A.OrdE α => OrdEInst

ordEInst :: Ord α => C.Type α -> OrdEInst α
ordEInst t =
  case t of
    C.Bool   _ -> error "ordEInst!"
    C.Int8   p -> mkInst p OrdEInst ; C.Int16  p -> mkInst p OrdEInst
    C.Int32  p -> mkInst p OrdEInst ; C.Int64  p -> mkInst p OrdEInst
    C.Word8  p -> mkInst p OrdEInst ; C.Word16 p -> mkInst p OrdEInst
    C.Word32 p -> mkInst p OrdEInst ; C.Word64 p -> mkInst p OrdEInst
    C.Float  p -> mkInst p OrdEInst ; C.Double p -> mkInst p OrdEInst

--------------------------------------------------------------------------------

data NumEInst α = A.NumE α => NumEInst

numEInst :: Num α => C.Type α -> NumEInst α
numEInst t =
  case t of
    C.Bool   _ -> error "numEInst!" -- !! this can't happen !!
    C.Int8   p -> mkInst p NumEInst ; C.Int16  p -> mkInst p NumEInst
    C.Int32  p -> mkInst p NumEInst ; C.Int64  p -> mkInst p NumEInst
    C.Word8  p -> mkInst p NumEInst ; C.Word16 p -> mkInst p NumEInst
    C.Word32 p -> mkInst p NumEInst ; C.Word64 p -> mkInst p NumEInst
    C.Float  p -> mkInst p NumEInst ; C.Double p -> mkInst p NumEInst

--------------------------------------------------------------------------------

data IntegralEInst α = A.IntegralE α => IntegralEInst

integralEInst :: Integral α => C.Type α -> IntegralEInst α
integralEInst t =
  case t of
    C.Bool   _ -> error "integralEInst!" -- !! this can't happen !!
    C.Int8   p -> mkInst p IntegralEInst ; C.Int16  p -> mkInst p IntegralEInst
    C.Int32  p -> mkInst p IntegralEInst ; C.Int64  p -> mkInst p IntegralEInst
    C.Word8  p -> mkInst p IntegralEInst ; C.Word16 p -> mkInst p IntegralEInst
    C.Word32 p -> mkInst p IntegralEInst ; C.Word64 p -> mkInst p IntegralEInst
    C.Float  _ -> error "integralEInst!" -- !! this can't happen !!
    C.Double _ -> error "integralEInst!" -- !! this can't happen !!

--------------------------------------------------------------------------------