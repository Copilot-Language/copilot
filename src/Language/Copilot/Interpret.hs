-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Language.Copilot.Interpret
  ( interpret
  ) where

import Control.DeepSeq (NFData, deepseq)
import Language.Copilot.Core
import Language.Copilot.Core.HeteroMap (HeteroMap, Key)
import qualified Language.Copilot.Core.HeteroMap as H

-- | Interprets a CoPilot-specification.
interpret
  -- Number of periods to evaluate:
  :: (HasType a)
  -- The specification to be evaluated:
  => WithSpec a
  -- The resulting list:
  -> [a]
interpret (WithSpec runSpec) =
  runSpec $ \ (Spec m n0) ->
    eval (continue m) n0

continue
  :: (HasType b, HeteroMap map)
  => map (CanonStream (Key map))
  -> Key map b
  -> [b]
continue m k = H.lookup k env
  where
    env = H.mapWithKey (const $ evalAnonym $ continue m) m

evalAnonym
  :: (forall b . HasType b => key b -> [b])
  -> CanonStream key a
  -> [a]
evalAnonym con a0 = case a0 of
  CanonStream _ xs n -> strict $ xs ++ eval con n

-- A continuation-style evaluator:
eval
  -- A continuation for evaluating the children of the node:
  :: (forall b . HasType b => f b -> [b])
  -> Expr f a -- The node to be evaluated.
  -> [a]      -- The resulting stream.
eval con n0 = case n0 of
  Const x         -> strict $ repeat x
  Drop i k        -> strict $ drop i (con k)
  Extern _        -> error "eval: Extern"
  Fun1 f k        -> strict $ fmap (fun1 f) (eval con k)
  Fun2 f k1 k2    -> strict $ zipWith  (fun2 f) (eval con k1) (eval con k2)
  Fun3 f k1 k2 k3 -> strict $ zipWith3 (fun3 f) (eval con k1) (eval con k2) (eval con k3)

strict :: NFData a => [a] -> [a]
strict (x : xs) = x `deepseq` (x : strict xs)
strict []       = []

fun1 :: Fun1 a b -> a -> b
fun1 Not     = not
fun1 Abs     = abs
fun1 Signum  = signum
fun1 Recip   = recip
fun1 Exp     = exp
fun1 Sqrt    = sqrt
fun1 Log     = log
fun1 Sin     = sin
fun1 Tan     = tan
fun1 Cos     = cos
fun1 Asin    = asin
fun1 Atan    = atan
fun1 Acos    = acos
fun1 Sinh    = sinh
fun1 Tanh    = tanh
fun1 Cosh    = cosh
fun1 Asinh   = asinh
fun1 Atanh   = atanh
fun1 Acosh   = acosh

fun2 :: Fun2 a b c -> a -> b -> c
fun2 And     = (&&)
fun2 Or      = (||)
fun2 Add     = (+)
fun2 Sub     = (-)
fun2 Mul     = (*)
fun2 Div     = (/)
fun2 Mod     = mod
fun2 Eq      = (==)
fun2 Ne      = (/=)
fun2 Lt      = (<)
fun2 Gt      = (>)
fun2 Le      = (<=)
fun2 Ge      = (>=)
fun2 Pow     = (**)
fun2 LogBase = logBase

fun3 :: Fun3 a b c d -> a -> b -> c -> d
fun3 Mux     = \ v x y -> if v then x else y
