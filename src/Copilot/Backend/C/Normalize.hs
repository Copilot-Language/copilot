module Copilot.Backend.C.Normalize
  ( normalize
  ) where

import Copilot.Core
import Copilot.Backend.C.Tmp

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
      n <- fresh ("arg"++(opname1 op))
      e' <- normexpr e
      return $ Local ty ty' n e' (Op1 op (Var ty n))

    Op2 op e1 e2 -> do
      n1 <- fresh ((opname2 op)++"L")
      n2 <- fresh ((opname2 op)++"R")
      e1' <- normexpr e1
      e2' <- normexpr e2
      let (ty,ty',ty'') = deop2 op
          loc1 = Local ty  ty'' n1 e1' loc2
          loc2 = Local ty' ty'' n2 e2' expr
          expr = Op2 op (Var ty n1) (Var ty' n2)
      return loc1

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
