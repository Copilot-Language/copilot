--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module Copilot.Compile.C99.C2A
  ( c2aExpr
  , c2aType
  ) where

import qualified Copilot.Compile.C99.Queue as Q
import qualified Copilot.Compile.C99.Witness as W
import Copilot.Compile.C99.MetaTable
import qualified Copilot.Core as C
import Copilot.Core.Type.Equality ((=~=), coerce, cong)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Language.Atom as A
import Prelude hiding (id)

--------------------------------------------------------------------------------

c2aExpr :: MetaTable -> (forall e . C.Expr e => e a) -> A.E a
c2aExpr meta e = c2aExpr_ e M.empty meta

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

data Local = forall a . Local
  { localAtomExpr :: A.E a
  , localType     :: C.Type a }

type Env = Map C.Name Local

--------------------------------------------------------------------------------

newtype C2AExpr a = C2AExpr
  { c2aExpr_ :: Env -> MetaTable -> A.E a }

newtype C2AOp1 a b = C2AOp1
  { c2aOp1 :: A.E a -> A.E b }

newtype C2AOp2 a b c = C2AOp2
  { c2aOp2 :: A.E a -> A.E b -> A.E c }

newtype C2AOp3 a b c d = C2AOp3
  { c2aOp3 :: A.E a -> A.E b -> A.E c -> A.E d }

--------------------------------------------------------------------------------

instance C.Expr C2AExpr where

  ----------------------------------------------------

  const _ x = C2AExpr $ \ _ _ ->

    A.Const x

  ----------------------------------------------------

  drop t i id = C2AExpr $ \ _ meta ->

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
        Just p = t2 =~= t1
      in
        case W.exprInst t2 of
          W.ExprInst ->
            coerce (cong p) (Q.lookahead (fromIntegral i) que)

  ----------------------------------------------------

  local t1 _ name e1 e2 = C2AExpr $ \ env meta ->

    let
      e1'  = c2aExpr_ e1 env meta
      env' = M.insert name (Local e1' t1) env
    in
      c2aExpr_ e2 env' meta

  ----------------------------------------------------

  var t1 name = C2AExpr $ \ env _ ->

    let
      Just local = M.lookup name env
    in
      case local of
        Local
          { localAtomExpr = e
          , localType     = t2
          } ->
            let
              Just p = t2 =~= t1
            in
              case W.exprInst t2 of
                W.ExprInst ->
                  coerce (cong p) e

  ----------------------------------------------------

  externVar t name = C2AExpr $ \ _ _ ->

    (A.value . A.var' name . c2aType) t

  ----------------------------------------------------

  externFun t name _ = C2AExpr $ \ _ meta ->

    let
      Just extFunInfo = M.lookup name (externFunInfoMap meta)
    in
      externFun1 t extFunInfo

    where

    externFun1 t1
      ExternFunInfo
        { externFunInfoVar  = var
        , externFunInfoType = t2
        } =
      let
        Just p = t2 =~= t1
      in
        case W.exprInst t2 of
          W.ExprInst ->
            coerce (cong p) (A.value var)

  ----------------------------------------------------

  externArray t1 t2 name e1 = C2AExpr $ \ env meta ->

    case ( W.integralEInst t1, W.exprInst t2 ) of
         ( W.IntegralEInst   , W.ExprInst    ) ->
            let
              arr = A.array' name (c2aType t2)
              idx = c2aExpr_ e1 env meta
            in
              arr A.!. idx

  ----------------------------------------------------

  op1 op e = C2AExpr $ \ env meta ->

    c2aOp1 op (c2aExpr_ e env meta)

  ----------------------------------------------------

  op2 op e1 e2 = C2AExpr $ \ env meta ->

    c2aOp2 op (c2aExpr_ e1 env meta) (c2aExpr_ e2 env meta)

  ----------------------------------------------------

  op3 op e1 e2 e3 = C2AExpr $ \ env meta ->

    c2aOp3 op (c2aExpr_ e1 env meta) (c2aExpr_ e2 env meta)
      (c2aExpr_ e3 env meta)

  ----------------------------------------------------

instance C.Op1 C2AOp1 where
  not     = C2AOp1                                                A.not_
  abs   t = C2AOp1 $ case W.numEInst      t of W.NumEInst      -> abs
  sign  t = C2AOp1 $ case W.numEInst      t of W.NumEInst      -> signum
  recip t = C2AOp1 $ case W.numEInst      t of W.NumEInst      -> recip
  exp   t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> exp
  sqrt  t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> sqrt
  log   t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> log
  sin   t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> sin
  tan   t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> tan
  cos   t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> cos
  asin  t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> asin
  atan  t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> atan
  acos  t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> acos
  sinh  t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> sinh
  tanh  t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> tanh
  cosh  t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> cosh
  asinh t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> asinh
  atanh t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> atanh
  acosh t = C2AOp1 $ case W.floatingEInst t of W.FloatingEInst -> acosh
  bwNot t = C2AOp1 $ case W.bitsEInst     t of W.BitsEInst     -> (A.complement)

instance C.Op2 C2AOp2 where
  and           = C2AOp2                                                (A.&&.)
  or            = C2AOp2                                                (A.||.)
  add      t    = C2AOp2 $ case W.numEInst      t of W.NumEInst      -> (+)
  sub      t    = C2AOp2 $ case W.numEInst      t of W.NumEInst      -> (-)
  mul      t    = C2AOp2 $ case W.numEInst      t of W.NumEInst      -> (*)
  div      t    = C2AOp2 $ case W.integralEInst t of W.IntegralEInst -> A.div_
  mod      t    = C2AOp2 $ case W.integralEInst t of W.IntegralEInst -> A.mod_
  fdiv     t    = C2AOp2 $ case W.numEInst      t of W.NumEInst      -> (/)
  pow      t    = C2AOp2 $ case W.floatingEInst t of W.FloatingEInst -> (**)
  logb     t    = C2AOp2 $ case W.floatingEInst t of W.FloatingEInst -> logBase
  eq       t    = C2AOp2 $ case W.eqEInst       t of W.EqEInst       -> (A.==.)
  ne       t    = C2AOp2 $ case W.eqEInst       t of W.EqEInst       -> (A./=.)
  le       t    = C2AOp2 $ case W.ordEInst      t of W.OrdEInst      -> (A.<=.)
  ge       t    = C2AOp2 $ case W.ordEInst      t of W.OrdEInst      -> (A.>=.)
  lt       t    = C2AOp2 $ case W.ordEInst      t of W.OrdEInst      -> (A.<.)
  gt       t    = C2AOp2 $ case W.ordEInst      t of W.OrdEInst      -> (A.>.)
  bwAnd    t    = C2AOp2 $ case W.bitsEInst     t of W.BitsEInst     -> (A..&.)
  bwOr     t    = C2AOp2 $ case W.bitsEInst     t of W.BitsEInst     -> (A..|.)
  bwXor    t    = C2AOp2 $ case W.bitsEInst     t of W.BitsEInst     -> (A.xor)
  bwShiftL t t' = C2AOp2 $ case ( W.bitsEInst t, W.integralEInst t' )
                           of ( W.BitsEInst, W.IntegralEInst )       -> (A..<<.)
  bwShiftR t t' = C2AOp2 $ case ( W.bitsEInst t, W.integralEInst t' )
                           of ( W.BitsEInst, W.IntegralEInst )       -> (A..>>.)

instance C.Op3 C2AOp3 where
  mux t      = C2AOp3 $ case W.exprInst        t of W.ExprInst        -> A.mux
