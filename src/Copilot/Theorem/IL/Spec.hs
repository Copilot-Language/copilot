--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs, LambdaCase #-}
{-# LANGUAGE Safe #-}

-- | This module implements the specification language for the IL format, an
-- intermediate representation used in copilot-theorem to facilitate model
-- checking.
--
-- A Copilot program is translated into a list of quantifier-free equations
-- over integer sequences, implicitly universally quantified by a free variable
-- n. Each sequence roughly corresponds to a stream.
--
-- This representation is partly inspired by the IL language described in
-- Hagen, G.E., /VERIFYING SAFETY PROPERTIES OF LUSTRE PROGRAMS: AN SMT-BASED
-- APPROACH/, 2008.

module Copilot.Theorem.IL.Spec
  ( Type (..)
  , Op1  (..)
  , Op2  (..)
  , SeqId
  , SeqIndex (..)
  , SeqDescr (..)
  , VarDescr (..)
  , Expr (..)
  , IL (..)
  , PropId
  , typeOf
  , _n_
  , _n_plus
  , evalAt
  ) where

import Data.Map (Map)
import Data.Function (on)

--------------------------------------------------------------------------------

type SeqId    =  String

data SeqIndex = Fixed Integer | Var Integer
  deriving (Eq, Ord, Show)

-- | Idealized types. These differ from Copilot types in that, notionally,
-- reals actually denote real numbers.
data Type = Bool  | Real
  | SBV8 | SBV16 | SBV32 | SBV64
  | BV8  | BV16 | BV32 | BV64
  deriving (Eq, Ord)

instance Show Type where
  show = \case
    Bool  -> "Bool"
    Real  -> "Real"
    SBV8  -> "SBV8"
    SBV16 -> "SBV16"
    SBV32 -> "SBV32"
    SBV64 -> "SBV64"
    BV8   -> "BV8"
    BV16  -> "BV16"
    BV32  -> "BV32"
    BV64  -> "BV64"

-- | Idealized representation of a Copilot expression.
data Expr
  = ConstB Bool
  | ConstR Double
  | ConstI Type Integer
  | Ite    Type Expr Expr Expr
  | Op1    Type Op1 Expr
  | Op2    Type Op2 Expr Expr
  | SVal   Type SeqId SeqIndex
  | FunApp Type String [Expr]
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

data VarDescr = VarDescr
  { varName :: String
  , varType :: Type
  , args    :: [Type]
  }

instance Eq VarDescr where
  (==) = (==) `on` varName

instance Ord VarDescr where
  compare = compare `on` varName

--------------------------------------------------------------------------------

type PropId = String

data SeqDescr = SeqDescr
  { seqId    :: SeqId
  , seqType  :: Type
  }

data IL = IL
  { modelInit   :: [Expr]
  , modelRec    :: [Expr]
  , properties  :: Map PropId ([Expr], Expr)
  , inductive   :: Bool
  }

--------------------------------------------------------------------------------

-- | Unary operators.
data Op1 = Not | Neg | Abs | Exp | Sqrt | Log | Sin | Tan | Cos | Asin | Atan
         | Acos | Sinh | Tanh | Cosh | Asinh | Atanh | Acosh
         deriving (Eq, Ord)

-- | Binary operators.
data Op2 = Eq | And | Or | Le | Lt | Ge | Gt | Add | Sub | Mul | Mod | Fdiv | Pow
         deriving (Eq, Ord)

-------------------------------------------------------------------------------

instance Show Op1 where
  show op = case op of
    Neg   -> "-"
    Not   -> "not"
    Abs   -> "abs"
    Exp   -> "exp"
    Sqrt  -> "sqrt"
    Log   -> "log"
    Sin   -> "sin"
    Tan   -> "tan"
    Cos   -> "cos"
    Asin  -> "asin"
    Atan  -> "atan"
    Acos  -> "acos"
    Sinh  -> "sinh"
    Tanh  -> "tanh"
    Cosh  -> "cosh"
    Asinh -> "asinh"
    Atanh -> "atanh"
    Acosh -> "acosh"

instance Show Op2 where
  show op = case op of
    And  -> "and"
    Or   -> "or"

    Add  -> "+"
    Sub  -> "-"
    Mul  -> "*"

    Mod  -> "mod"

    Fdiv -> "/"

    Pow  -> "^"

    Eq   -> "="

    Le   -> "<="
    Ge   -> ">="
    Lt   -> "<"
    Gt   -> ">"

-------------------------------------------------------------------------------

-- | Return the type of an expression.
typeOf :: Expr -> Type
typeOf e = case e of
  ConstB _       -> Bool
  ConstR _       -> Real
  ConstI t _     -> t
  Ite    t _ _ _ -> t
  Op1    t _ _   -> t
  Op2    t _ _ _ -> t
  SVal   t _ _   -> t
  FunApp t _ _   -> t

_n_ :: SeqIndex
_n_ = Var 0

_n_plus :: (Integral a) => a -> SeqIndex
_n_plus d = Var (toInteger d)

evalAt :: SeqIndex -> Expr -> Expr
evalAt _ e@(ConstB _) = e
evalAt _ e@(ConstR _) = e
evalAt _ e@(ConstI _ _) = e
evalAt i (Op1 t op e) = Op1 t op (evalAt i e)
evalAt i (Op2 t op e1 e2) = Op2 t op (evalAt i e1) (evalAt i e2)
evalAt i (Ite t c e1 e2) = Ite t (evalAt i c) (evalAt i e1) (evalAt i e2)
evalAt i (FunApp t name args) = FunApp t name $ map (\e -> evalAt i e) args

evalAt _ e@(SVal _ _ (Fixed _)) = e
evalAt (Fixed n) (SVal t s (Var d)) = SVal t s (Fixed $ n + d)
evalAt (Var   k) (SVal t s (Var d)) = SVal t s (Var   $ k + d)

--------------------------------------------------------------------------------
