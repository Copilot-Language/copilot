--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}

module Copilot.Compile.C99.C2A
  ( c2aExpr
  , c2aType
  ) where

import qualified Copilot.Compile.C99.Queue as Q
import qualified Copilot.Compile.C99.Witness as W
import Copilot.Compile.C99.MetaTable
import qualified Copilot.Core as C
import Copilot.Core.Type.Equality ((=~=), coerce, cong)
import qualified Data.Map as M
import qualified Language.Atom as A
import Prelude hiding (id)

--------------------------------------------------------------------------------

c2aExpr :: MetaTable -> (forall η . C.Expr η => η a) -> A.E a
c2aExpr m e = c2aExpr_ e m

--------------------------------------------------------------------------------

c2aType :: C.Type a -> A.Type
c2aType t =
  case t of
    C.Bool   _ -> A.Bool
    C.Int8   _ -> A.Int8   ; C.Int16  _ -> A.Int16
    C.Int32  _ -> A.Int32  ; C.Int64  _ -> A.Int64
    C.Word8  _ -> A.Word8  ; C.Word16 _ -> A.Word16
    C.Word32 _ -> A.Word32 ; C.Word64 _ -> A.Word64
    C.Float  _ -> A.Float  ; C.Double _ -> A.Double

--------------------------------------------------------------------------------

newtype C2AExpr a = C2AExpr
  { c2aExpr_ :: MetaTable -> A.E a }

newtype C2AOp1 a b = C2AOp1
  { c2aOp1 :: A.E a -> A.E b }

newtype C2AOp2 a b c = C2AOp2
  { c2aOp2 :: A.E a -> A.E b -> A.E c }

newtype C2AOp3 a b c d = C2AOp3
  { c2aOp3 :: A.E a -> A.E b -> A.E c -> A.E d }

--------------------------------------------------------------------------------

instance C.Expr C2AExpr where

  ----------------------------------------------------

  const _ x = C2AExpr $ \ _ ->

    A.Const x

  ----------------------------------------------------

  drop t i id = C2AExpr $ \ meta ->

    let
      Just strmInfo = M.lookup id (streamInfoMap meta)
    in
      drop1 t strmInfo

    where

    drop1 :: C.Type a -> StreamInfo -> A.E a
    drop1 t1
      StreamInfo
        { streamInfoQueue = que
        , streamInfoType  = t2
        } =
      let
        Just p = (=~=) t2 t1
      in
        case W.exprInst t2 of
          W.ExprInst ->
            coerce (cong p) (Q.lookahead (fromIntegral i) que)

  ----------------------------------------------------

  extern t cs = C2AExpr $ \ _ ->

    (A.value . A.var' cs . c2aType) t

  ----------------------------------------------------

  op1 op e = C2AExpr $ \ meta ->

    let
      e' = c2aExpr_ e meta
    in
      c2aOp1 op e'

  ----------------------------------------------------

  op2 op e1 e2 = C2AExpr $ \ meta ->

    let
      e1' = c2aExpr_ e1 meta
      e2' = c2aExpr_ e2 meta
    in
      c2aOp2 op e1' e2'

  ----------------------------------------------------

  op3 op e1 e2 e3 = C2AExpr $ \ meta ->

    let
      e1' = c2aExpr_ e1 meta
      e2' = c2aExpr_ e2 meta
      e3' = c2aExpr_ e3 meta
    in
      c2aOp3 op e1' e2' e3'

  ----------------------------------------------------

instance C.Op1 C2AOp1 where
  not     = C2AOp1                                                    A.not_
  abs   t = C2AOp1 $ case W.numEInst        t of W.NumEInst        -> abs
  sign  t = C2AOp1 $ case W.numEInst        t of W.NumEInst        -> signum
  recip t = C2AOp1 $ case W.numEInst        t of W.NumEInst        -> recip
  exp   t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> exp
  sqrt  t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> sqrt
  log   t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> log
  sin   t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> sin
  tan   t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> tan
  cos   t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> cos
  asin  t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> asin
  atan  t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> atan
  acos  t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> acos
  sinh  t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> sinh
  tanh  t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> tanh
  cosh  t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> cosh
  asinh t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> asinh
  atanh t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> atanh
  acosh t = C2AOp1 $ case W.floatingEInst   t of W.FloatingEInst   -> acosh


instance C.Op2 C2AOp2 where
  and     = C2AOp2                                                    (A.&&.)
  or      = C2AOp2                                                    (A.||.)
  add   t = C2AOp2 $ case W.numEInst        t of W.NumEInst        -> (+)
  sub   t = C2AOp2 $ case W.numEInst        t of W.NumEInst        -> (-)
  mul   t = C2AOp2 $ case W.numEInst        t of W.NumEInst        -> (*)
  div   t = C2AOp2 $ case W.integralEInst   t of W.IntegralEInst   -> A.div_
  mod   t = C2AOp2 $ case W.integralEInst   t of W.IntegralEInst   -> A.mod_
  fdiv  t = C2AOp2 $ case W.numEInst        t of W.NumEInst        -> (/)
  pow   t = C2AOp2 $ case W.floatingEInst   t of W.FloatingEInst   -> (**)
  logb  t = C2AOp2 $ case W.floatingEInst   t of W.FloatingEInst   -> logBase
  eq    t = C2AOp2 $ case W.eqEInst         t of W.EqEInst         -> (A.==.)
  ne    t = C2AOp2 $ case W.eqEInst         t of W.EqEInst         -> (A./=.)
  le    t = C2AOp2 $ case W.ordEInst        t of W.OrdEInst        -> (A.<=.)
  ge    t = C2AOp2 $ case W.ordEInst        t of W.OrdEInst        -> (A.>=.)
  lt    t = C2AOp2 $ case W.ordEInst        t of W.OrdEInst        -> (A.<.)
  gt    t = C2AOp2 $ case W.ordEInst        t of W.OrdEInst        -> (A.>.)

instance C.Op3 C2AOp3 where
  mux t   = C2AOp3 $ case W.exprInst        t of W.ExprInst        -> A.mux

--------------------------------------------------------------------------------