-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Language.Copilot.Interpret
  ( interpret
  ) where

import Control.DeepSeq (NFData, deepseq)
import Language.Copilot.Core
import Prelude hiding (lookup, map)

-- | Interprets a CoPilot-specification.
interpret
  -- Number of periods to evaluate:
  :: (Streamable a, Specification spec)
  => Integer
  -- The specification to be evaluated:
  -> spec a
  -- The resulting list:
  -> [a]
interpret n spec =
  runSpec spec $ \ m k0 ->
    let
      env = fmap2 (eval $ evalKey env) m
    in
      take (fromInteger n) $ strict $ hlookup k0 env

evalKey
  :: (HeteroMap map, Typed a)
  => map []
  -> Key map a
  -> [a]
evalKey = flip hlookup

-- A continuation-style evaluator:
eval
  -- A continuation for evaluating the children of the node:
  :: (forall b . Typed b => f b -> [b])
  -> Node f a -- The node to be evaluated.
  -> [a]      -- The resulting stream.
eval con n0 = case n0 of
  Const x         -> repeat x
  Append xs k     -> xs ++ con k-- foldr (S.<:>) (con k) xs
  Drop i k        -> drop i (con k)
  Fun1 f k        -> fmap     (fun1 f) (con k)
  Fun2 f k1 k2    -> zipWith  (fun2 f) (con k1) (con k2)
  Fun3 f k1 k2 k3 -> zipWith3 (fun3 f) (con k1) (con k2) (con k3)
  Extern _        -> error "eval: Extern"

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
fun2 Index   = flip indexArray
fun2 Pow     = (**)
fun2 LogBase = logBase

fun3 :: Fun3 a b c d -> a -> b -> c -> d
fun3 Mux     = \ v x y -> if v then x else y
