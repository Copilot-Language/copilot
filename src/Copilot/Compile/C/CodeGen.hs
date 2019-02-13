{-# LANGUAGE GADTs #-}

module Copilot.Compile.C.CodeGen where

import Control.Monad.State

import Copilot.Core
import Copilot.Compile.C.Util

import qualified Language.C99.Simple as C

-- | Translates a Copilot expression into a C99 expression.
transexpr :: Expr a -> State FunEnv C.Expr
transexpr (Const ty x) = return $ constty ty x

transexpr (Local ty1 _ name e1 e2) = do
  e1' <- transexpr e1
  e2' <- transexpr e2
  statetell ([C.Decln Nothing (transtype ty1) name (C.InitExpr e1')], [])
  return $ e2'

transexpr (Var _ n) = return $ C.Ident n

transexpr (Drop _ _ sid) = do
  (declns, vars) <- get
  let usednames = (names declns) ++ vars
      freshname = fresh name usednames
      name = case sid of
        0 -> "drop"
        n -> "drop_" ++ show n
  return $ C.Ident freshname

transexpr (ExternVar _ name _) = return $ C.Ident (cpyname name)

transexpr (ExternFun _ _ _ _ _) = undefined

transexpr (Label _ _ _) = undefined

transexpr (Op1 op e) = do
  e' <- transexpr e
  return $ transop1 op e'

transexpr (Op2 op e1 e2) = do
  e1' <- transexpr e1
  e2' <- transexpr e2
  return $ transop2 op e1' e2'

transexpr (Op3 op e1 e2 e3) = do
  e1' <- transexpr e1
  e2' <- transexpr e2
  e3' <- transexpr e3
  return $ transop3 op e1' e2' e3'


-- | Translates a Copilot unary operator and arguments into a C99 expression.
transop1 :: Op1 a b -> C.Expr -> C.Expr
transop1 = undefined

-- | Translates a Copilot binary operator and arguments into a C99 expression.
transop2 :: Op2 a b c -> C.Expr -> C.Expr -> C.Expr
transop2 = undefined

-- | Translates a Copilot ternaty operator and arguments into a C99 expression.
transop3 :: Op3 a b c d -> C.Expr -> C.Expr -> C.Expr -> C.Expr
transop3 = undefined

-- | Give a C99 literal expression based on a value and a type.
constty :: Type a -> a -> C.Expr
constty = undefined

-- | Translate a Copilot type to a C99 type.
transtype :: Type a -> C.Type
transtype = undefined
