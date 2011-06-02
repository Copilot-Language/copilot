-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | An tagless interpreter for Copilot specifications.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Interpret
  ( interpret
  ) where

import Data.List (transpose)
import Copilot.Core
import Copilot.Core.Type.Dynamic
import Prelude hiding (id)
import qualified Prelude as P
import Text.PrettyPrint.NCol (asColumns)
import Text.PrettyPrint (Doc, text, render)

--

type Env = [(Int, DynamicF [] Type)]

--

newtype EvalExpr α         = EvalExpr { evalExpr :: Env -> [α] }
newtype EvalOp1  α β       = EvalOp1  { evalOp1  :: α -> β }
newtype EvalOp2  α β γ     = EvalOp2  { evalOp2  :: α -> β -> γ }
newtype EvalOp3  α β γ δ   = EvalOp3  { evalOp3  :: α -> β -> γ -> δ }

--

class Strict f where
  strict :: f α -> f α

instance Strict [] where
  strict []     = []
  strict (x:xs) = x `seq` (x : strict xs)

instance Strict EvalExpr where
  strict e = EvalExpr $ \ m -> strict (evalExpr e m)

-- EvalExpr is an applicative functor:

pure :: α -> EvalExpr α
pure x = EvalExpr $ \ _ -> repeat x

(<*>) :: EvalExpr (α -> β) -> EvalExpr α -> EvalExpr β
e1 <*> e2 = EvalExpr $ \ env -> zipWith ($) (evalExpr e1 env) (evalExpr e2 env)

--

instance Expr EvalExpr where
  const _ x          = x `seq` pure x
  drop t i id        = strict $ EvalExpr $ \ env ->
                         case lookup id env >>= fromDynamicF t of
                           Just xs -> P.drop (fromIntegral i) xs
                           Nothing -> error "interpret: drop!"
  extern _ _         = error "interpret: extern!"
  op1 op e1          = strict $ pure (evalOp1 op) <*> e1
  op2 op e1 e2       = strict $ pure (evalOp2 op) <*> e1 <*> e2
  op3 op e1 e2 e3    = strict $ pure (evalOp3 op) <*> e1 <*> e2 <*> e3

instance Op1 EvalOp1 where
  not      = EvalOp1 P.not
  abs _    = EvalOp1 P.abs
  sign _   = EvalOp1 signum

instance Op2 EvalOp2 where
  and      = EvalOp2 (&&)
  or       = EvalOp2 (||)
  add _    = EvalOp2 (+)
  sub _    = EvalOp2 (-)
  mul _    = EvalOp2 (*)
  mod _    = EvalOp2 P.mod
  div _    = EvalOp2 P.div
  eq _     = EvalOp2 (==)
  ne _     = EvalOp2 (/=)
  le _     = EvalOp2 (<=)
  ge _     = EvalOp2 (>=)
  lt _     = EvalOp2 (<)
  gt _     = EvalOp2 (>)

instance Op3 EvalOp3 where
  mux _    = EvalOp3 $ \ v x y -> if v then x else y

--

type Output = (Bool, String)

withGuard :: α -> [Bool] -> [α] -> [α]
withGuard _ (True:vs)  (x:xs) = x : withGuard x vs xs
withGuard z (False:vs) xs     = z : withGuard z vs xs
withGuard _ _          _      = error "withGuard: empty stream."

evalStream :: Env -> Stream -> (Int, DynamicF [] Type)
evalStream env (Stream t id buffer mguard e1) =
    (id, toDynamicF ys t)
  where
    xs = buffer ++ evalExpr e1 env
    ys =
      case mguard of
        Just e2 -> withGuard (uninitialized t) (evalExpr e2 env) xs
        Nothing -> xs

evalTrigger :: Env -> Trigger -> [Output]
evalTrigger env (Trigger _ _ guard value) =
    zip bs (fmap show vs)
  where
    bs = evalExpr guard env
    vs = evalExpr value env

evalSpec :: Spec -> [[Output]]
evalSpec spec = out
  where
    env = fmap (evalStream  env) (specStreams  spec)
    out = fmap (evalTrigger env) (specTriggers spec)

-- | Interprets a Copilot specification.
interpret :: Int -> Spec -> String
interpret k spec =
  ( render
  . asColumns
  . transpose
  . (:) (ppTriggerNames $ specTriggers spec)
  . take k
  . ppOutputs
  . evalSpec
  ) spec

-- Functions for pretty-printing the output of the interpreter:

ppTriggerNames :: [Trigger] -> [Doc]
ppTriggerNames = map (text . (++ ":") . triggerName)

ppOutput :: Output -> Doc
ppOutput (True,  v) = text v
ppOutput (False, _) = text "--"

ppOutputs :: [[Output]] -> [[Doc]]
ppOutputs [] = []
ppOutputs xs = map (ppOutput . head) xs : ppOutputs (map tail xs)
