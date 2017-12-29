{-# LANGUAGE GADTs #-}

module Copilot.Backend.C.Tmp where

-- All temporary stuff, that needs some final location / implementation

import Copilot.Core
import Language.C99.AST as C
import Language.C99.Util

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
  BwNot _  -> "bnot"
  Cast _ _ -> "(cast)"


opname2 :: Op2 a b c -> String
opname2 op = case op of
  And          ->  "and"
  Or           ->  "or"
  Add      _   ->  "plus"
  Sub      _   ->  "min"
  Mul      _   ->  "mult"
  Div      _   ->  "div"
  Mod      _   ->  "mod"
  Fdiv     _   ->  "fdiv"
  Pow      _   ->  "pow"
  Logb     _   ->  "logb"
  Eq       _   ->  "Eq"
  Ne       _   ->  "NEq"
  Le       _   ->  "LE"
  Ge       _   ->  "GE"
  Lt       _   ->  "LT"
  Gt       _   ->  "GT"
  BwAnd    _   ->  "BAnd"
  BwOr     _   ->  "BOR"
  BwXor    _   ->  "BXOR"
  BwShiftL _ _ ->  "LS"
  BwShiftR _ _ ->  "RS"


var n = EIdent $ ident n

funcall :: String -> [C.Expr] -> C.Expr
funcall n es = EFunCall (EIdent $ ident n) (args es) where
  args [] = Nothing
  args (x:[]) = Just $ AELBase x
  args (x:xs) = Just $ foldl (\xs x -> AELCons xs x) (AELBase x) xs

constint x = EConst $ cint (fromIntegral x)
constword x = EConst $ cuint (fromIntegral x)
constfloat x = EConst $ cfloat (fromIntegral x)
constbool b = EConst $ cbool b



fundef :: String -> DeclnSpecs -> CompoundStmt -> FunDef
fundef n ds body = FD ds dr Nothing body where
  dr = Dr Nothing (DDIdent $ ident n)
