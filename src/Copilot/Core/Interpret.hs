-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | An tagless interpreter for Copilot specifications.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Interpret
  ( interpret
  ) where

import Data.List (transpose)
import Copilot.Core
import Copilot.Core.Type
import Copilot.Core.Type.Dynamic
import Copilot.Core.Saturable
import Copilot.Core.Uninitializable
import Prelude hiding (abs, drop, and, div, mod, or)
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
newtype EvalOp4  α β γ δ ξ = EvalOp4  { evalOp4  :: α -> β -> γ -> δ -> ξ }

--

class Strict f where
  strict :: Saturable α => f α -> f α

instance Strict [] where
  strict []     = []
  strict (x:xs) = saturate x `seq` (x : strict xs)

instance Strict EvalExpr where
  strict e = EvalExpr $ \ m -> strict (evalExpr e m)

-- EvalExpr is an applicative functor:

pure :: α -> EvalExpr α
pure x = EvalExpr $ \ _ -> repeat x

(<*>) :: EvalExpr (α -> β) -> EvalExpr α -> EvalExpr β
e1 <*> e2 = EvalExpr $ \ env -> zipWith ($) (evalExpr e1 env) (evalExpr e2 env)

--

instance Expr EvalExpr where
  const x            = saturate x `seq` pure x
  drop i id_         = strict $ EvalExpr $ \ env ->
                        case lookup id_ env >>= fromDynamicF typeOf of
                          Just xs -> P.drop i xs
                          Nothing -> error "interpret: drop!"
  extern _           = error "interpret: extern!"
  op1 op e1          = strict $ pure (evalOp1 op) <*> e1
  op2 op e1 e2       = strict $ pure (evalOp2 op) <*> e1 <*> e2
  op3 op e1 e2 e3    = strict $ pure (evalOp3 op) <*> e1 <*> e2 <*> e3
  op4 op e1 e2 e3 e4 = strict $ pure (evalOp4 op) <*> e1 <*> e2 <*> e3 <*> e4

instance Op1 EvalOp1 where
  abs      = EvalOp1 P.abs
  not      = EvalOp1 P.not
  sign     = EvalOp1 signum
  untup2_1 = EvalOp1 fst
  untup2_2 = EvalOp1 snd
  untup3_1 = EvalOp1 $ \ (x, _, _) -> x
  untup3_2 = EvalOp1 $ \ (_, y, _) -> y
  untup3_3 = EvalOp1 $ \ (_, _, z) -> z
  untup4_1 = EvalOp1 $ \ (x, _, _, _) -> x
  untup4_2 = EvalOp1 $ \ (_, y, _, _) -> y
  untup4_3 = EvalOp1 $ \ (_, _, z, _) -> z
  untup4_4 = EvalOp1 $ \ (_, _, _, w) -> w

instance Op2 EvalOp2 where
  and      = EvalOp2 (&&)
  or       = EvalOp2 (||)
  add      = EvalOp2 (+)
  sub      = EvalOp2 (-)
  mul      = EvalOp2 (*)
  mod      = EvalOp2 P.mod
  div      = EvalOp2 P.div
  eq       = EvalOp2 (==)
  ne       = EvalOp2 (/=)
  le       = EvalOp2 (<=)
  ge       = EvalOp2 (>=)
  lt       = EvalOp2 (<)
  gt       = EvalOp2 (>)
  tup2     = EvalOp2 (,)

instance Op3 EvalOp3 where
  mux      = EvalOp3 $ \ v x y -> if v then x else y
  tup3     = EvalOp3 (,,)

instance Op4 EvalOp4 where
  tup4     = EvalOp4 (,,,)

--

type Output = (Bool, String)

withGuard :: α -> [Bool] -> [α] -> [α]
withGuard _ (True:vs)  (x:xs) = x : withGuard x vs xs
withGuard z (False:vs) xs     = z : withGuard z vs xs
withGuard _ _          _      = error "withGuard: empty stream."

evalStream :: Env -> Stream -> (Int, DynamicF [] Type)
evalStream env (Stream id_ buffer mguard e1) =
    (id_, toDynamicF ys typeOf)
  where
    xs = buffer ++ evalExpr e1 env
    ys =
      case mguard of
        Just e2 -> withGuard uninitialized (evalExpr e2 env) xs
        Nothing -> xs

evalTrigger :: Env -> Trigger -> [Output]
evalTrigger env (Trigger _ guard value) =
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

--
-- Pretty-print the output of the interpreter:
--

ppTriggerNames :: [Trigger] -> [Doc]
ppTriggerNames = map (text . (++ ":") . triggerName)

ppOutput :: Output -> Doc
ppOutput (True,  v) = text v
ppOutput (False, _) = text "--"

ppOutputs :: [[Output]] -> [[Doc]]
ppOutputs [] = []
ppOutputs xs = map (ppOutput . head) xs : ppOutputs (map tail xs)
