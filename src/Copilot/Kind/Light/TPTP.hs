--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Copilot.Kind.MetiTarski.TPTP where

import Copilot.Kind.IL
import Copilot.Kind.Misc.SExpr
import Copilot.Kind.Misc.Error as Err

import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe

--------------------------------------------------------------------------------

data TPTPStmt = Ax TPTPExpr | Conj TPTPExpr

data TPTPExpr = Infix TPTPExpr String TPTPExpr
              | Atom String | Fun String [String]

type Term = SExpr String

smtTy :: Type t -> String
smtTy Integer = "Int"
smtTy Real    = "Real"
smtTy Bool    = "Bool"

--------------------------------------------------------------------------------

axiom :: Constraint -> TPTPStmt
axiom c = node "assert" [expr c]

conjecture :: Constraint -> TPTPStmt
conjecture c = node "assert" [expr c]

checkSat :: Term
checkSat = singleton "check-sat"

setLogic :: String -> Term
setLogic l = node "set-logic" [atom l]

--------------------------------------------------------------------------------

uexpr :: U Expr -> Term
uexpr (U e) = expr e

expr :: Expr t -> Term

expr (Const Integer v) = atom (show v)
expr (Const Bool    b) = atom (if b then "true" else "false")
expr (Const Real    v) = atom (show v)

expr (Ite _ cond e1 e2) = node "ite" [expr cond, expr e1, expr e2]

expr (FunApp _ funName args) = node funName $ map uexpr args

expr (Op1 _ op e) =
  node smtOp [expr e]
  where
    smtOp = case op of
      Not -> "not"
      Neg -> "-"

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

expr (SVal _ f ix) = case ix of
      Fixed i -> atom $ f ++ "_" ++ show i
      Var off -> atom $ f ++ "_n" ++ show off

--------------------------------------------------------------------------------
