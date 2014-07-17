--------------------------------------------------------------------------------

module Copilot.Kind.Light.SMTLib where

import Copilot.Kind.IL
import Copilot.Kind.Misc.SExpr
import Copilot.Kind.Misc.Error as Err

import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe

--------------------------------------------------------------------------------

type Term = SExpr String

smtTy :: Type t -> String
smtTy Integer = "Int"
smtTy Real    = "Real"
smtTy Bool    = "Bool"

varN :: String
varN = "n"

--------------------------------------------------------------------------------

declConst :: Type a -> String -> Term
declConst t id = node "declare-fun" [atom id, unit, atom $ smtTy t]

declSeq :: Type a -> String -> Term
declSeq t id = node "declare-fun"
                [atom id, singleton (smtTy Integer), atom $ smtTy t]
                
declFun :: String -> Type t -> [U Type] -> Term
declFun name retTy argsTy = 
  node "declare-fun" 
    [ atom name
    , list $ map (\(U t) -> atom $ smtTy t) argsTy
    , atom (smtTy retTy) ]

assert :: Constraint -> Term
assert c = node "assert" [expr c]

push :: Term
push = node "push" [atom "1"]

pop :: Term
pop = node "pop" [atom "1"]

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

expr (Ite _ cond e1 e2) = node ("ite")
                             [expr cond, expr e1, expr e2]

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

expr (SVal _ f ix) = node f [_ix]

  where _ix = case ix of
          Fixed i -> atom (show i)
          Var off -> node "+"
                     [atom varN, atom (show off)]

--------------------------------------------------------------------------------
