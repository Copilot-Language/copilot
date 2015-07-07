--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Copilot.Kind.Misc.Operators where

--------------------------------------------------------------------------------

data Op1 a b where
  Not   :: Op1 Bool Bool
  Neg   :: Op1 t t
  Abs   :: Op1 t t
  Exp   :: Op1 t t
  Sqrt  :: Op1 t t
  Log   :: Op1 t t
  Sin   :: Op1 t t
  Tan   :: Op1 t t
  Cos   :: Op1 t t
  Asin  :: Op1 t t
  Atan  :: Op1 t t
  Acos  :: Op1 t t
  Sinh  :: Op1 t t
  Tanh  :: Op1 t t
  Cosh  :: Op1 t t
  Asinh :: Op1 t t
  Atanh :: Op1 t t
  Acosh :: Op1 t t

data Op2 a b c where
  Eq     :: Op2 a    a    Bool
  And    :: Op2 Bool Bool Bool
  Or     :: Op2 Bool Bool Bool

  Le     :: (Num t) => Op2 t t Bool
  Lt     :: (Num t) => Op2 t t Bool
  Ge     :: (Num t) => Op2 t t Bool
  Gt     :: (Num t) => Op2 t t Bool

  Add    :: (Num t) => Op2 t t t
  Sub    :: (Num t) => Op2 t t t
  Mul    :: (Num t) => Op2 t t t
  FDiv   :: (Num t) => Op2 t t t

  Pow    :: (Num t) => Op2 t t t

-------------------------------------------------------------------------------

instance Show (Op1 a b) where
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

instance Show (Op2 a b c) where
  show op = case op of
    Eq   -> "="
    Le   -> "<="
    Lt   -> "<"
    Ge   -> ">="
    Gt   -> ">"
    And  -> "and"
    Or   -> "or"
    Add  -> "+"
    Sub  -> "-"
    Mul  -> "*"
    FDiv -> "/"
    Pow  -> "^"

-------------------------------------------------------------------------------
