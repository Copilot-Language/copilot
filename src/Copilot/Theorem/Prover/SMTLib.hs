--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, FlexibleInstances #-}

module Copilot.Theorem.Prover.SMTLib (SmtLib, interpret) where

import Copilot.Theorem.Prover.Backend (SmtFormat (..), SatResult (..))

import Copilot.Theorem.IL
import Copilot.Theorem.Misc.SExpr

import Text.Printf

--------------------------------------------------------------------------------

newtype SmtLib = SmtLib (SExpr String)

instance Show SmtLib where
  show (SmtLib s) = show s

smtTy :: Type -> String
smtTy Bool    = "Bool"
smtTy Real    = "Real"
smtTy _       = "Int"

--------------------------------------------------------------------------------

instance SmtFormat SmtLib where
  push = SmtLib $ node "push" [atom "1"]
  pop = SmtLib $ node "pop" [atom "1"]
  checkSat = SmtLib $ singleton "check-sat"
  setLogic "" = SmtLib $ blank
  setLogic l = SmtLib $ node "set-logic" [atom l]
  declFun name retTy args = SmtLib $
    node "declare-fun" [atom name, (list $ map (atom . smtTy) args), atom (smtTy retTy)]
  assert c = SmtLib $ node "assert" [expr c]

interpret :: String -> Maybe SatResult
interpret "sat"   = Just Sat
interpret "unsat" = Just Unsat
interpret _       = Just Unknown

--------------------------------------------------------------------------------

expr :: Expr -> SExpr String

expr (ConstB v) = atom $ if v then "true" else "false"
expr (ConstI _ v) = atom $ show v
expr (ConstR v) = atom $ printf "%f" v

expr (Ite _ cond e1 e2) = node "ite" [expr cond, expr e1, expr e2]

expr (FunApp _ funName args) = node funName $ map expr args

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
      Mod  -> "mod"
      Fdiv -> "/"
      Pow  -> "^"

expr (SVal _ f ix) = atom $ case ix of
  Fixed i -> f ++ "_" ++ show i
  Var off -> f ++ "_n" ++ show off

--------------------------------------------------------------------------------
