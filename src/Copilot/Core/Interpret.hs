-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | An tagless interpreter for Copilot specifications.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Interpret
  ( interpret
  ) where

import Control.DeepSeq (NFData, deepseq)
import Data.List (intersperse)
import Copilot.Core
import Copilot.Core.Type.Dynamic
import Prelude hiding (abs, drop, and, div, mod, or)
import qualified Prelude as P

type Env = [(Int, DynamicF [] Type)]

--

newtype EvalExpr α       = EvalExpr { evalExpr :: Env -> [α] }
newtype EvalOp1  α β     = EvalOp1  { evalOp1  :: α -> β }
newtype EvalOp2  α β γ   = EvalOp2  { evalOp2  :: α -> β -> γ }
newtype EvalOp3  α β γ δ = EvalOp3  { evalOp3  :: α -> β -> γ -> δ }

--

strict :: NFData α => EvalExpr α -> EvalExpr α
strict e = EvalExpr $ \ m -> forceList (evalExpr e m)
  where
    forceList (x:xs) = x `deepseq` (x : forceList xs)
    forceList []     = []

-- EvalExpr is an applicative functor:

pure :: α -> EvalExpr α
pure x = EvalExpr $ \ _ -> repeat x

(<*>) :: EvalExpr (α -> β) -> EvalExpr α -> EvalExpr β
e1 <*> e2 = EvalExpr $ \ env -> zipWith ($) (evalExpr e1 env) (evalExpr e2 env)

withEnv :: (Env -> [α]) -> EvalExpr α
withEnv = EvalExpr

--

instance Expr EvalExpr where
  const x         = x `deepseq` pure x
  drop i id_      = strict $ withEnv $ \ env ->
                      case lookup id_ env >>= fromDynamicF typeOf of
                        Just xs -> P.drop i xs
                        Nothing -> error "interpret: drop!"
  extern _        = error "interpret: extern!"
  op1 op e1       = strict $ pure (evalOp1 op) <*> e1
  op2 op e1 e2    = strict $ pure (evalOp2 op) <*> e1 <*> e2
  op3 op e1 e2 e3 = strict $ pure (evalOp3 op) <*> e1 <*> e2 <*> e3

instance Op1 EvalOp1 where
  abs  = EvalOp1 P.abs
  not  = EvalOp1 P.not
  sign = EvalOp1 signum

instance Op2 EvalOp2 where
  and  = EvalOp2 (&&)
  or   = EvalOp2 (||)
  add  = EvalOp2 (+)
  sub  = EvalOp2 (-)
  mul  = EvalOp2 (*)
  mod  = EvalOp2 P.mod
  div  = EvalOp2 P.div
  eq   = EvalOp2 (==)
  ne   = EvalOp2 (/=)
  le   = EvalOp2 (<=)
  ge   = EvalOp2 (>=)
  lt   = EvalOp2 (<)
  gt   = EvalOp2 (>)

instance Op3 EvalOp3 where
  mux  = EvalOp3 $ \ v x y -> if v then x else y

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

-- | Interprets a CoPilot specification.
interpret :: Int -> Spec -> String
interpret k spec =
  unlines $
    ppTriggerNames (specTriggers spec) :
    take k (ppOutputs (evalSpec spec))

--

type Line = String

ppTriggerNames :: [Trigger] -> Line
ppTriggerNames =
  concat . intersperse " " . map (extend16 . (++ ":") . triggerName)

ppOutput :: Output -> String
ppOutput (True,  v) = extend16 v
ppOutput (False, _) = extend16 "--"

ppOutputs :: [[Output]] -> [Line]
ppOutputs [] = []
ppOutputs xs =
  (concat . intersperse " " . map (ppOutput . head) $ xs)
    : ppOutputs (map tail xs)

extend16 :: String -> String
extend16 cs | length cs < 16 = cs ++ replicate (16 - length cs) ' '
extend16 cs | length cs > 16 = take 13 cs ++ "..."
extend16 cs | otherwise      = cs
