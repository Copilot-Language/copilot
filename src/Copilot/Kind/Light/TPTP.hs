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
  show (Ax e) = "fof(formula, axiom, " ++ show e ++ ")."
  show Null   = ""

instance Show TptpExpr where
  show (Bin e1 op e2)  = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
  show (Un op e)       = "(" ++ op ++ " " ++ show e ++ ")"
  show (Atom atom)     = atom
  show (Fun name args) = name ++ "(" ++ intercalate ", " (map show args) ++ ")"

--------------------------------------------------------------------------------

instance SmtFormat Tptp where
  push     = Null
  pop      = Null
  checkSat = Null
  setLogic = const Null
  declFun  = const $ const $ const Null
  assert c = Ax $ expr c

interpret :: String -> Maybe SatResult
interpret str
  | "SZS status Unsatisfiable" `isPrefixOf` str = Just Unsat
  | "SZS status"               `isPrefixOf` str = Just Unknown
  | otherwise                                   = Nothing

--------------------------------------------------------------------------------

expr :: Expr -> TptpExpr
expr = \case
  ConstB v        -> Atom $ if v then "$true" else "$false"
  ConstI v        -> Atom $ show v
  ConstR v        -> Atom $ show v

  Ite _ c e1 e2   -> Bin (Bin (expr c) "=>" (expr e1))
                             "&" (Bin (Un "~" (expr c)) "=>" (expr e2))

  FunApp _ f args -> Fun f $ map expr args

  Op1 _ Not e     -> Un (showOp1 Not) $ expr e
  Op1 _ Neg e     -> Un (showOp1 Neg) $ expr e
  Op1 _ op e      -> Fun (showOp1 op) [expr e]

  Op2 _ op e1 e2  -> Bin (expr e1) (showOp2 op) (expr e2)

  SVal _ f ix     -> case ix of
                       Fixed i -> Atom $ f ++ "_" ++ show i
                       Var off -> Atom $ f ++ "_n" ++ show off

showOp1 :: Op1 -> String
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

showOp2 :: Op2 -> String
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
  Mod   -> "mod"
  Fdiv  -> "/"
  Pow   -> "^"

--------------------------------------------------------------------------------
