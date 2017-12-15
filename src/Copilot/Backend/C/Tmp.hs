{-# LANGUAGE GADTs #-}

module Copilot.Backend.C.Tmp where

-- All temporary stuff, that needs some final location / implementation

import Copilot.Core

-- TODO: find better solution to fix this problem
deop :: Op1 a b -> (Type a,Type b)
deop op = let bi x = (x,x) in case op of
  Not     -> bi Bool
  Abs ty  -> bi ty
  Exp ty  -> bi ty
  -- TODO

deop2 :: Op2 a b c -> (Type a, Type b, Type c)
deop2 op = let tri x = (x,x,x) in case op of
  And     -> tri Bool
  Or      -> tri Bool
  Add ty  -> tri ty
  -- TODO

deop3 :: Op3 a b c d -> (Type a, Type b, Type c, Type d)
deop3 (Mux ty) = (Bool, ty, ty, ty)




opname1 :: Op1 a b -> String
opname1 op = case op of
  Not      -> "not"
  Abs _    -> "abs"
  Sign _   -> "signum"
  Recip _  -> "recip"
  Exp _    -> "exp"
  Sqrt _   -> "sqrt"
  Log _    -> "log"
  Sin _    -> "sin"
  Tan _    -> "tan"
  Cos _    -> "cos"
  Asin _   -> "asin"
  Atan _   -> "atan"
  Acos _   -> "acos"
  Sinh _   -> "sinh"
  Tanh _   -> "tanh"
  Cosh _   -> "cosh"
  Asinh _  -> "asinh"
  Atanh _  -> "atanh"
  Acosh _  -> "acosh"
  BwNot _  -> "~"
  Cast _ _ -> "(cast)"


opname2 :: Op2 a b c -> String
opname2 op = case op of
  And          ->  "&&"
  Or           ->  "||"
  Add      _   ->  "+"
  Sub      _   ->  "-"
  Mul      _   ->  "*"
  Div      _   ->  "div"
  Mod      _   ->  "mod"
  Fdiv     _   ->  "/"
  Pow      _   ->  "**"
  Logb     _   ->  "logBase"
  Eq       _   ->  "=="
  Ne       _   ->  "/="
  Le       _   ->  "<="
  Ge       _   ->  ">="
  Lt       _   ->  "<"
  Gt       _   ->  ">"
  BwAnd    _   ->  "&"
  BwOr     _   ->  "|"
  BwXor    _   ->  "^"
  BwShiftL _ _ ->  "<<"
  BwShiftR _ _ ->  ">>"

