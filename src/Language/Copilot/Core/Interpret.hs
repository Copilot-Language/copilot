-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | An tagless interpreter for Copilot specifications.

{-# LANGUAGE Rank2Types #-}

module Language.Copilot.Core.Interpret
  ( interpret
  ) where

import Language.Copilot.Core
import qualified Language.Copilot.Core.HeteroMap as H

--

newtype EvalExpr α = EvalExpr
  { evalExpr
      :: forall φ . H.Map φ
      => φ []
      -> [α]
  }

newtype EvalOp1 α β     = EvalOp1 { evalOp1 :: α -> β }
newtype EvalOp2 α β γ   = EvalOp2 { evalOp2 :: α -> β -> γ }
newtype EvalOp3 α β γ δ = EvalOp3 { evalOp3 :: α -> β -> γ -> δ }

--

strict :: EvalExpr α -> EvalExpr α
strict e = EvalExpr $ \ m -> force (evalExpr e m)
  where
    force (x:xs) = x `seq` (x : force xs)
    force []     = []

-- EvalExpr is an applicative functor:

pure :: α -> EvalExpr α
pure x = EvalExpr $ \ _ -> return x

(<*>) :: EvalExpr (α -> β) -> EvalExpr α -> EvalExpr β
η1 <*> η2 = EvalExpr $ \ φ -> zipWith ($) (evalExpr η1 φ) (evalExpr η2 φ)

--

instance Expr EvalExpr where
  drp i key      = strict $ EvalExpr $ \ φ ->
                     case H.lookup key φ of
                       Just xs -> drop i xs
                       Nothing -> error "interpret: drp!"
  lit x          = x `seq` pure x
  ext _          = error "interpret: ext!"
  op1 θ η1       = strict $ pure (evalOp1 θ) <*> η1
  op2 θ η1 η2    = strict $ pure (evalOp2 θ) <*> η1 <*> η2
  op3 θ η1 η2 η3 = strict $ pure (evalOp3 θ) <*> η1 <*> η2 <*> η3

instance Op1 EvalOp1 where
  abs'    = EvalOp1 abs
  not'    = EvalOp1 not
  signum' = EvalOp1 signum

instance Op2 EvalOp2 where
  (&&.) = EvalOp2 (&&)
  (||.) = EvalOp2 (||)
  (+.)  = EvalOp2 (+)
  (-.)  = EvalOp2 (-)
  (*.)  = EvalOp2 (*)
  mod'  = EvalOp2 mod
  (==.) = EvalOp2 (==)
  (/=.) = EvalOp2 (/=)
  (>=.) = EvalOp2 (>=)
  (<=.) = EvalOp2 (<=)
  (>.)  = EvalOp2 (>)
  (<.)  = EvalOp2 (<)

instance Op3 EvalOp3 where
  if_then_else = EvalOp3 $ \ v x y -> if v then x else y

--

evalStrm :: H.Map φ => φ [] -> Strm α -> [α]
evalStrm φ (Strm buffer e) = buffer ++ evalExpr e φ

evalTrig :: H.Map φ => φ [] -> Trig -> (Name, [String])
evalTrig φ (Trig name e1 e2) = (name, fmap show (evalExpr e1 φ))

evalSpec :: Spec -> [(Name, [String])]
evalSpec (Spec streams triggers) = y
  where
    φ = H.map (evalStrm φ) streams
    y = fmap  (evalTrig φ) triggers

-- | Interprets a CoPilot specification.
interpret :: Spec -> String
interpret spec = undefined
