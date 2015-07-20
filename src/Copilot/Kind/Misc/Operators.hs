--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Copilot.Kind.Misc.Operators where

--------------------------------------------------------------------------------

data Op1 a b where
  Not   :: Op1 Bool Bool
  Neg   :: Op1 a a
  Abs   :: Op1 a a
  Exp   :: Op1 a a
  Sqrt  :: Op1 a a
  Log   :: Op1 a a
  Sin   :: Op1 a a
  Tan   :: Op1 a a
  Cos   :: Op1 a a
  Asin  :: Op1 a a
  Atan  :: Op1 a a
  Acos  :: Op1 a a
  Sinh  :: Op1 a a
  Tanh  :: Op1 a a
  Cosh  :: Op1 a a
  Asinh :: Op1 a a
  Atanh :: Op1 a a
  Acosh :: Op1 a a

data Op2 a b c where
  Eq     :: Op2 a    a    Bool
  And    :: Op2 Bool Bool Bool
  Or     :: Op2 Bool Bool Bool

  Le     :: (Num a) => Op2 a a Bool
  Lt     :: (Num a) => Op2 a a Bool
  Ge     :: (Num a) => Op2 a a Bool
  Gt     :: (Num a) => Op2 a a Bool

  Add    :: (Num a) => Op2 a a a
  Sub    :: (Num a) => Op2 a a a
  Mul    :: (Num a) => Op2 a a a
  Mod    :: (Num a) => Op2 a a a
  FDiv   :: (Num a) => Op2 a a a

  Pow    :: (Num a) => Op2 a a a

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
    Mod  -> "mod"
    FDiv -> "/"
    Pow  -> "^"

-------------------------------------------------------------------------------
