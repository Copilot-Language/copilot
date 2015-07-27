--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Copilot.Kind.Misc.Operators where

--------------------------------------------------------------------------------

data Op1 a where
  Not   :: Op1 Bool
  Neg   :: Op1 a
  Abs   :: Op1 a
  Exp   :: Op1 a
  Sqrt  :: Op1 a
  Log   :: Op1 a
  Sin   :: Op1 a
  Tan   :: Op1 a
  Cos   :: Op1 a
  Asin  :: Op1 a
  Atan  :: Op1 a
  Acos  :: Op1 a
  Sinh  :: Op1 a
  Tanh  :: Op1 a
  Cosh  :: Op1 a
  Asinh :: Op1 a
  Atanh :: Op1 a
  Acosh :: Op1 a

data Op2 a b where
  Eq     :: Op2 a    Bool
  And    :: Op2 Bool Bool
  Or     :: Op2 Bool Bool

  Le     :: (Num a) => Op2 a Bool
  Lt     :: (Num a) => Op2 a Bool
  Ge     :: (Num a) => Op2 a Bool
  Gt     :: (Num a) => Op2 a Bool

  Add    :: (Num a) => Op2 a a
  Sub    :: (Num a) => Op2 a a
  Mul    :: (Num a) => Op2 a a
  Mod    :: (Num a) => Op2 a a
  FDiv   :: (Num a) => Op2 a a

  Pow    :: (Num a) => Op2 a a

-------------------------------------------------------------------------------

instance Show (Op1 a) where
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

instance Show (Op2 a b) where
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
    Mod  -> "mod"
    FDiv -> "/"
    Pow  -> "^"

-------------------------------------------------------------------------------
