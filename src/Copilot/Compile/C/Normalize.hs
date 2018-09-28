{-# LANGUAGE GADTs #-}

module Copilot.Compile.C.Normalize
  ( normalize
  ) where

import Copilot.Core
import Copilot.Compile.C.Tmp

import Data.Typeable          (Typeable)
import Control.Monad.State

type VarName = String
type Env = State [VarName]

{- Rewrite all subexpressions of a specification into a normal form.
   The normal form is the Copilot.Core expression equivalent of ANormal Form.
-}
normalize :: Spec -> Spec
normalize s = evalState (normalize' s) [] where
  normalize' :: Spec -> Env Spec
  normalize' s = do
    streams' <- mapM normstream (specStreams s)
    observers' <- mapM normobserver (specObservers s)
    triggers' <- mapM normtrigger (specTriggers s)
    return $ Spec streams' observers' triggers' (specProperties s)

  normstream :: Stream -> Env Stream
  normstream (Stream id buff expr ty) = do
    expr' <- normexpr expr
    return $ Stream id buff expr' ty

  normobserver :: Observer -> Env Observer
  normobserver (Observer name expr ty) = do
    expr' <- normexpr expr
    return $ Observer name expr' ty

  normtrigger :: Trigger -> Env Trigger
  normtrigger (Trigger name guard args) = do
    guard' <- normexpr guard
    args' <- mapM normuexpr args
    return $ Trigger name guard' args'

  normexpr :: Expr a -> Env (Expr a)
  normexpr e = case e of
    Local ty1 ty2 n e1 e2 -> do
      e1' <- normexpr e1
      e2' <- normexpr e2
      n' <- fresh n
      return $ Local ty1 ty2 n' e1' e2'

    -- TODO
    ExternFun ty n ues me mt -> undefined

    Op1 op e -> do
      let (ty,ty') = deop op
      (arg, loc) <- oparg e ty ty' ""
      return $ loc (Op1 op arg)

    Op2 op e1 e2 -> do
      let (ty, ty', ty'') = deop2 op
      (arg0, loc0) <- oparg e1 ty  ty'' "L"
      (arg1, loc1) <- oparg e2 ty' ty'' "R"
      return $ (loc0 . loc1) (Op2 op arg0 arg1)


    Op3 op e1 e2 e3 -> do
      n1 <- fresh "cond"
      n2 <- fresh "thenb"
      n3 <- fresh "elseb"
      e1' <- normexpr e1
      e2' <- normexpr e2
      e3' <- normexpr e3
      let (ty,ty',ty'',ty''') = deop3 op
          loc1 = Local ty   ty''' n1 e1' loc2
          loc2 = Local ty'  ty''' n2 e2' loc3
          loc3 = Local ty'' ty''' n3 e3' expr
          expr = Op3 op (Var ty n1) (Var ty' n2) (Var ty'' n3)
      return loc1

    x -> return x

  normuexpr :: UExpr -> Env UExpr
  normuexpr (UExpr ty expr) = do
    expr' <- normexpr expr
    return $ UExpr ty expr'

  fresh :: VarName -> Env VarName
  fresh v = do
    used <- get
    let v' = head $ dropWhile (flip elem used) freshvars
        freshvars = v:((v ++).show <$> [0..])
    put (v':used)
    return v'

  -- In case the expression is a terminal we do not want to create a local
  -- variable out of it.
  -- If it is not a terminal, return a variable as argument to the operator and
  -- create a local definition which can be chained.
  oparg :: Typeable a
    => Expr a -> Type a -> Type b -> String -> Env (Expr a, (Expr b -> Expr b))
  oparg e ty ty' postfix = do
    let opname (Op1 op _    ) = opname1 op
        opname (Op2 op _ _  ) = opname2 op
    e' <- normexpr e
    if isterminal e'
      then    return (e, id)
      else do n <- fresh ((opname e) ++ postfix)
              return (Var ty n, \e'' -> Local ty ty' n e' e'')

-- Is the expression a terminal in the AST?
isterminal :: Expr a -> Bool
isterminal (Drop _ 0 _) = True
isterminal (Const _ _)  = True
isterminal (Var _ _)    = True
isterminal _            = False

