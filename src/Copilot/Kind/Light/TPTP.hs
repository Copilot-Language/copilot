--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, LambdaCase #-}

module Copilot.Kind.Light.TPTP (Tptp, interpret) where

import Copilot.Kind.Light.Backend (SmtFormat (..), SatResult (..))
import Copilot.Kind.IL

import Data.List

--------------------------------------------------------------------------------

data Tptp = Ax TptpExpr | Null

data TptpExpr = Bin TptpExpr String TptpExpr | Un String TptpExpr
              | Atom String | Fun String [TptpExpr]

instance Show Tptp where
  show (Ax expr) = "fof(formula, axiom, " ++ show expr ++ ")."
  show Null      = ""

instance Show TptpExpr where
  show (Bin expr1 op expr2) = "(" ++ show expr1 ++ op ++ show expr2 ++ ")"
  show (Un op expr) = "(" ++ op ++ show expr ++ ")"
  show (Atom atom) = atom
  show (Fun name args) = name ++ "(" ++ intercalate ", " (map show args) ++ ")"

--------------------------------------------------------------------------------

instance SmtFormat Tptp where
  push = Null
  pop = Null
  checkSat = Null
  setLogic = const Null
  declFun = const $ const $ const Null
  assert c = Ax $ expr c

interpret :: String -> Maybe SatResult
interpret str
  | "SZS status Unsatisfiable" `isPrefixOf` str = Just Unsat
  | "SZS status"               `isPrefixOf` str = Just Unknown
  | otherwise                                   = Nothing

--------------------------------------------------------------------------------

uexpr :: U Expr -> TptpExpr
uexpr (U e) = expr e

expr :: Expr t -> TptpExpr

expr (Const Integer v) = Atom $ show v
expr (Const Bool b) = Atom $ if b then "true" else "false"
expr (Const Real v) = Atom $ show v

expr (Ite _ cond expr1 expr2) = Bin (Bin (expr cond) "=>" (expr expr1))
  "&" (Bin (Un "~" (expr cond)) "=>" (expr expr2))

expr (FunApp _ funName args) = Fun funName $ map uexpr args

expr (Op1 _ Not e) = Un (showOp1 Not) $ expr e
expr (Op1 _ Neg e) = Un (showOp1 Neg) $ expr e
expr (Op1 _ op e) = Fun (showOp1 op) [expr e]

expr (Op2 _ op expr1 expr2) = Bin (expr expr1) (showOp2 op) (expr expr2)

expr (SVal _ f ix) = case ix of
      Fixed i -> Atom $ f ++ "_" ++ show i
      Var off -> Atom $ f ++ "_n" ++ show off

showOp1 :: Op1 a b -> String
showOp1 = \case
  Not   -> "~"
  Neg   -> "-"
  Abs   -> "abs"
  Exp   -> "exp"
  Sqrt  -> "sqrt"
  Log   -> "log"
  Sin   -> "sin"
  Tan   -> "tan"
  Cos   -> "cos"
  Asin  -> "arcsin"
  Atan  -> "arctan"
  Acos  -> "arccos"
  Sinh  -> "sinh"
  Tanh  -> "tanh"
  Cosh  -> "cosh"
  Asinh -> "arcsinh"
  Atanh -> "arctanh"
  Acosh -> "arccosh"

showOp2 :: Op2 a b c -> String
showOp2 = \case
  Eq    -> "="
  Le    -> "<="
  Lt    -> "<"
  Ge    -> ">="
  Gt    -> ">"
  And   -> "&"
  Or    -> "|"
  Add   -> "+"
  Sub   -> "-"
  Mul   -> "*"
  FDiv  -> "/"
  Pow   -> "^"

--------------------------------------------------------------------------------
