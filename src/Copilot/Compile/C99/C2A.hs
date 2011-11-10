--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Copilot.Compile.C99.C2A
  ( c2aExpr
  , c2aType
  ) where

import qualified Copilot.Compile.C99.Queue as Q
import qualified Copilot.Compile.C99.Witness as W
import Copilot.Compile.C99.MetaTable
import Copilot.Core (Op1 (..), Op2 (..), Op3 (..))
import qualified Copilot.Core as C
import Copilot.Core.Type.Equality ((=~=), coerce, cong)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Language.Atom as A
import qualified Prelude as P
import Prelude hiding (id)

--------------------------------------------------------------------------------

c2aExpr :: MetaTable -> C.Expr a -> A.E a
c2aExpr meta e = c2aExpr_ e M.empty meta

--------------------------------------------------------------------------------

c2aType :: C.Type a -> A.Type
c2aType t =
  case t of
    C.Bool   -> A.Bool
    C.Int8   -> A.Int8   ; C.Int16  -> A.Int16
    C.Int32  -> A.Int32  ; C.Int64  -> A.Int64
    C.Word8  -> A.Word8  ; C.Word16 -> A.Word16
    C.Word32 -> A.Word32 ; C.Word64 -> A.Word64
    C.Float  -> A.Float  ; C.Double -> A.Double

--------------------------------------------------------------------------------

data Local = forall a . Local
  { localAtomExpr :: A.E a
  , localType     :: C.Type a }

type Env = Map C.Name Local

--------------------------------------------------------------------------------

c2aExpr_ :: C.Expr a -> Env -> MetaTable -> A.E a
c2aExpr_ e0 env meta = case e0 of

  ----------------------------------------------------

  C.Const _ x ->

    A.Const x

  ----------------------------------------------------

  C.Drop t i id ->

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

  C.Local t1 _ name e1 e2 ->

    let
      e1'  = c2aExpr_ e1 env meta
      env' = M.insert name (Local e1' t1) env
    in
      c2aExpr_ e2 env' meta

  ----------------------------------------------------

  C.Var t1 name ->

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

  C.ExternVar t name ->

    let
      Just externInfo = M.lookup name (externInfoMap meta)
    in
      externVar1 t externInfo

    where

    externVar1 :: C.Type a -> ExternInfo -> A.E a
    externVar1 t1
      ExternInfo
        { externInfoVar  = v
        , externInfoType = t2
        } =
      let
        Just p = t2 =~= t1
      in
        coerce (cong p) (A.value v)

  ----------------------------------------------------

  C.ExternFun t name _ (Just tag) ->

    let
      Just extFunInfo = M.lookup (name, tag) (externFunInfoMap meta)
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

  C.ExternArray _ t name _ (Just tag) ->

    let
      Just extArrayInfo = M.lookup (name, tag) (externArrayInfoMap meta)
    in
      externArray1 t extArrayInfo

    where

    externArray1 t1
      ExternArrayInfo
        { externArrayInfoVar      = var
        , externArrayInfoElemType = t2
        } =
      let
        Just p = t2 =~= t1
      in
        case W.exprInst t2 of
          W.ExprInst ->
            coerce (cong p) (A.value var)

  ----------------------------------------------------

  C.Op1 op e ->

    c2aOp1 op (c2aExpr_ e env meta)

  ----------------------------------------------------

  C.Op2 op e1 e2 ->

    c2aOp2 op (c2aExpr_ e1 env meta) (c2aExpr_ e2 env meta)

  ----------------------------------------------------

  C.Op3 op e1 e2 e3 ->

    c2aOp3 op (c2aExpr_ e1 env meta) (c2aExpr_ e2 env meta)
      (c2aExpr_ e3 env meta)

  ----------------------------------------------------

c2aOp1 :: C.Op1 a b -> A.E a -> A.E b
c2aOp1 op = case op of
  Not     ->                                                  A.not_
  Abs   t -> case W.numEInst        t of W.NumEInst        -> abs
  Sign  t -> case W.numEInst        t of W.NumEInst        -> signum
  Recip t -> case W.numEInst        t of W.NumEInst        -> recip
  Exp   t -> case W.floatingEInst   t of W.FloatingEInst   -> exp
  Sqrt  t -> case W.floatingEInst   t of W.FloatingEInst   -> sqrt
  Log   t -> case W.floatingEInst   t of W.FloatingEInst   -> log
  Sin   t -> case W.floatingEInst   t of W.FloatingEInst   -> sin
  Tan   t -> case W.floatingEInst   t of W.FloatingEInst   -> tan
  Cos   t -> case W.floatingEInst   t of W.FloatingEInst   -> cos
  Asin  t -> case W.floatingEInst   t of W.FloatingEInst   -> asin
  Atan  t -> case W.floatingEInst   t of W.FloatingEInst   -> atan
  Acos  t -> case W.floatingEInst   t of W.FloatingEInst   -> acos
  Sinh  t -> case W.floatingEInst   t of W.FloatingEInst   -> sinh
  Tanh  t -> case W.floatingEInst   t of W.FloatingEInst   -> tanh
  Cosh  t -> case W.floatingEInst   t of W.FloatingEInst   -> cosh
  Asinh t -> case W.floatingEInst   t of W.FloatingEInst   -> asinh
  Atanh t -> case W.floatingEInst   t of W.FloatingEInst   -> atanh
  Acosh t -> case W.floatingEInst   t of W.FloatingEInst   -> acosh
  BwNot t -> case W.bitsEInst       t of W.BitsEInst       -> (A.complement)
  Cast C.Bool C.Bool ->                                       P.id
  Cast C.Bool t -> case W.numEInst  t of 
                     W.NumEInst     -> \e -> A.mux e (A.Const 1) (A.Const 0)
  Cast t0 t1    -> case W.numEInst  t0 of 
                     W.NumEInst     -> 
                       case W.numEInst t1 of W.NumEInst    -> A.Cast

c2aOp2 :: C.Op2 a b c -> A.E a -> A.E b -> A.E c
c2aOp2 op = case op of
  And     ->                                                  (A.&&.)
  Or      ->                                                  (A.||.)
  Add   t -> case W.numEInst        t of W.NumEInst        -> (+)
  Sub   t -> case W.numEInst        t of W.NumEInst        -> (-)
  Mul   t -> case W.numEInst        t of W.NumEInst        -> (*)
  Div   t -> case W.integralEInst   t of W.IntegralEInst   -> A.div_
  Mod   t -> case W.integralEInst   t of W.IntegralEInst   -> A.mod_
  Fdiv  t -> case W.numEInst        t of W.NumEInst        -> (/)
  Pow   t -> case W.floatingEInst   t of W.FloatingEInst   -> (**)
  Logb  t -> case W.floatingEInst   t of W.FloatingEInst   -> logBase
  Eq    t -> case W.eqEInst         t of W.EqEInst         -> (A.==.)
  Ne    t -> case W.eqEInst         t of W.EqEInst         -> (A./=.)
  Le    t -> case W.ordEInst        t of W.OrdEInst        -> (A.<=.)
  Ge    t -> case W.ordEInst        t of W.OrdEInst        -> (A.>=.)
  Lt    t -> case W.ordEInst        t of W.OrdEInst        -> (A.<.)
  Gt    t -> case W.ordEInst        t of W.OrdEInst        -> (A.>.)
  BwAnd t -> case W.bitsEInst       t of W.BitsEInst       -> (A..&.)
  BwOr  t -> case W.bitsEInst       t of W.BitsEInst       -> (A..|.)
  BwXor t -> case W.bitsEInst       t of W.BitsEInst       -> (A.xor)
  BwShiftL t t' -> case ( W.bitsEInst t, W.integralEInst t' )
                   of ( W.BitsEInst, W.IntegralEInst )      -> (A..<<.)
  BwShiftR t t' -> case ( W.bitsEInst t, W.integralEInst t' )
                   of ( W.BitsEInst, W.IntegralEInst )      -> (A..>>.)

c2aOp3 :: C.Op3 a b c d -> A.E a -> A.E b -> A.E c -> A.E d
c2aOp3 op = case op of
  Mux t   -> case W.exprInst        t of W.ExprInst        -> A.mux
