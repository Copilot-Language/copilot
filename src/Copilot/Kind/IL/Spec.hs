--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs, LambdaCase #-}

module Copilot.Kind.IL.Spec
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

type Offset   =  Integer

data SeqIndex = Fixed Integer | Var Offset
  deriving (Eq, Ord, Show)

data Type = Bool | Real | Integer
  deriving (Eq, Ord)

instance Show Type where
  show = \case
    Bool    -> "Bool"
    Real    -> "Real"
    Integer -> "Int"

data Expr
  = ConstB Bool
  | ConstR Double
  | ConstI Integer
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

data Op1 = Not | Neg | Abs | Exp | Sqrt | Log | Sin | Tan | Cos | Asin | Atan
         | Acos | Sinh | Tanh | Cosh | Asinh | Atanh | Acosh
         deriving (Eq, Ord)

data Op2 = Eq | And | Or | Le | Lt | Ge | Gt | Add | Sub | Mul | Mod | Fdiv
         | Pow
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

typeOf :: Expr -> Type
typeOf e = case e of
  ConstB _       -> Bool
  ConstR _       -> Real
  ConstI _       -> Integer
  Ite    t _ _ _   -> t
  Op1    t _ _     -> t
  Op2    t _ _ _   -> t
  SVal   t _ _     -> t
  FunApp t _ _     -> t

_n_ :: SeqIndex
_n_ = Var 0

_n_plus :: (Integral a) => a -> SeqIndex
_n_plus d = Var (toInteger d)

evalAt :: SeqIndex -> Expr -> Expr
evalAt _ (ConstB e) = ConstB e
evalAt _ (ConstR e) = ConstR e
evalAt _ (ConstI e) = ConstI e
evalAt i (Op1 t op e) = Op1 t op (evalAt i e)
evalAt i (Op2 t op e1 e2) = Op2 t op (evalAt i e1) (evalAt i e2)
evalAt i (Ite t c e1 e2) = Ite t (evalAt i c) (evalAt i e1) (evalAt i e2)
evalAt i (FunApp t name args) = FunApp t name $ map (\e -> evalAt i e) args

evalAt _ e@(SVal _ _ (Fixed _)) = e
evalAt (Fixed n) (SVal t s (Var d)) = SVal t s (Fixed $ n + d)
evalAt (Var   k) (SVal t s (Var d)) = SVal t s (Var   $ k + d)

--------------------------------------------------------------------------------
