{-# LANGUAGE LambdaCase #-}

module Copilot.Theorem.IL.Transform ( bsimpl ) where

import Copilot.Theorem.IL.Spec

-- | A transformation intended to remove boolean literals.
bsimpl :: Expr -> Expr
bsimpl = until (\x -> bsimpl' x == x) bsimpl'
  where
    bsimpl' = \case
      Ite _ (ConstB True) e _     -> bsimpl' e
      Ite _ (ConstB False) _ e    -> bsimpl' e
      Ite t c e1 e2               -> Ite t (bsimpl' c) (bsimpl' e1) (bsimpl' e2)

      Op1 _ Not (Op1 _ Not e)     -> bsimpl' e
      Op1 _ Not (ConstB True)     -> ConstB False
      Op1 _ Not (ConstB False)    -> ConstB True
      Op1 t o e                   -> Op1 t o (bsimpl' e)

      Op2 _ Or e (ConstB False)   -> bsimpl' e
      Op2 _ Or (ConstB False) e   -> bsimpl' e
      Op2 _ Or _ (ConstB True)    -> ConstB True
      Op2 _ Or (ConstB True) _    -> ConstB True

      Op2 _ And _ (ConstB False)  -> ConstB False
      Op2 _ And (ConstB False) _  -> ConstB False
      Op2 _ And e (ConstB True)   -> bsimpl' e
      Op2 _ And (ConstB True) e   -> bsimpl' e

      Op2 _ Eq e (ConstB False)   -> bsimpl' (Op1 Bool Not e)
      Op2 _ Eq (ConstB False) e   -> bsimpl' (Op1 Bool Not e)
      Op2 _ Eq e (ConstB True)    -> bsimpl' e
      Op2 _ Eq (ConstB True) e    -> bsimpl' e

      Op2 t o e1 e2               -> Op2 t o (bsimpl' e1) (bsimpl' e2)

      FunApp t f args             -> FunApp t f (map bsimpl' args)

      e                           -> e

