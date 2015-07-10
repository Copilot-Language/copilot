--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, FlexibleInstances #-}

module Copilot.Kind.Light.SMTLib (SmtLib, interpret) where

import Copilot.Kind.Light.Backend (SmtFormat (..), SatResult (..))

import Copilot.Kind.IL
import Copilot.Kind.Misc.SExpr

--------------------------------------------------------------------------------

newtype SmtLib = SmtLib (SExpr String)

instance Show SmtLib where
  show (SmtLib s) = show s

usmtTy :: U Type -> String
usmtTy (U t) = smtTy t

smtTy :: Type t -> String
smtTy Integer = "Int"
smtTy Real    = "Real"
smtTy Bool    = "Bool"

--------------------------------------------------------------------------------

instance SmtFormat SmtLib where
  push = SmtLib $ node "push" [atom "1"]
  pop = SmtLib $ node "pop" [atom "1"]
  checkSat = SmtLib $ singleton "check-sat"
  setLogic l = SmtLib $ node "set-logic" [atom l]
  declFun name retTy args = SmtLib $
    node "declare-fun" [atom name, (list $ map (atom . usmtTy) args), atom (smtTy retTy)]
  assert c = SmtLib $ node "assert" [expr c]

interpret :: String -> Maybe SatResult
interpret "sat"   = Just Sat
interpret "unsat" = Just Unsat
interpret _       = Just Unknown

--------------------------------------------------------------------------------

uexpr :: U Expr -> SExpr String
uexpr (U e) = expr e

expr :: Expr t -> SExpr String

expr (Const Integer v) = atom (show v)
expr (Const Bool    b) = atom (if b then "true" else "false")
expr (Const Real    v) = atom (show v)

expr (Ite _ cond e1 e2) = node "ite" [expr cond, expr e1, expr e2]

expr (FunApp _ funName args) = node funName $ map uexpr args

expr (Op1 _ op e) =
  node smtOp [expr e]
  where
    smtOp = case op of
      Not   -> "not"
      Neg   -> "-"
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

expr (Op2 _ op e1 e2) =
  node smtOp [expr e1, expr e2]
  where
    smtOp = case op of
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

expr (SVal _ f ix) = atom $ case ix of
      Fixed i -> f ++ "_" ++ show i
      Var off -> f ++ "_n" ++ show off

--------------------------------------------------------------------------------
