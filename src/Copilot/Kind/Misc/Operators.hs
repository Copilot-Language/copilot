--------------------------------------------------------------------------------

module Copilot.Kind.Misc.Operators where

--------------------------------------------------------------------------------

data Op1 a b where
  Not    :: Op1 Bool Bool
  Neg    :: Op1 Integer Integer

data Op2 a b c where
  EqB    :: Op2 Bool    Bool    Bool
  EqI    :: Op2 Integer Integer Bool
  Le     :: Op2 Integer Integer Bool
  Lt     :: Op2 Integer Integer Bool
  Ge     :: Op2 Integer Integer Bool
  Gt     :: Op2 Integer Integer Bool

  And    :: Op2 Bool    Bool    Bool
  Or     :: Op2 Bool    Bool    Bool

  Add    :: Op2 Integer Integer Integer
  Sub    :: Op2 Integer Integer Integer
  Mul    :: Op2 Integer Integer Integer

-------------------------------------------------------------------------------

instance Show (Op1 a b) where
  show op = case op of
    Neg -> "-"
    Not -> "not"

instance Show (Op2 a b c) where
  show op = case op of
    EqB -> "="
    EqI -> "="
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
