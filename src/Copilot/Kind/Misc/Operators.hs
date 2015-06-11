--------------------------------------------------------------------------------

module Copilot.Kind.Misc.Operators where

--------------------------------------------------------------------------------

data Op1 a b where
  Not    :: Op1 Bool Bool
  Neg    :: (Num t) => Op1 t t

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

-------------------------------------------------------------------------------

instance Show (Op1 a b) where
  show op = case op of
    Neg -> "-"
    Not -> "not"

instance Show (Op2 a b c) where
  show op = case op of
    Eq  -> "="
    Le  -> "<="
    Lt  -> "<"
    Ge  -> ">="
    Gt  -> ">"
    And -> "and"
    Or  -> "or"
    Add -> "+"
    Mul -> "*"
    Sub -> "-"

-------------------------------------------------------------------------------
