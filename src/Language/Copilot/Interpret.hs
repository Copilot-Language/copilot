-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Copilot.Interpret
  ( interpret
  ) where

-- !! Has nothing to do with Language.Copilot.Interface.Stream !!
-- See 'http://hackage.haskell.org/package/Stream'.
-- The reason we use streams (instead lists) in the evaluator
-- is to obviate the need for dealing with list lengths.
import Data.Stream (Stream)
import qualified Data.Stream as S

import Control.Applicative (Applicative ((<*>)), (<$>))
import Language.Copilot.Node (Node (..), Fun1 (..), Fun2 (..), Fun3 (..))
import Language.Copilot.Array (Array (..))
import Language.Copilot.Spec (Spec (..), lookup, fmap2)
import Language.Copilot.Streamable (Streamable)
import Prelude hiding (lookup)

-- | Interprets a CoPilot-specification.
interpret
  :: Streamable a
  -- Number of periods to evaluate:
  => Integer
  -- The specification to be evaluated:
  -> Spec a
  -- The resulting list:
  -> [a]
interpret n (Spec m k0) = S.take (fromInteger n) $ lookup k0 env
  where
    env = fmap2 (eval (\ k -> lookup k env)) m

-- A continuation-style evaluator.
eval
  -- A continuation for evaluating the children of the node:
  :: (forall b . Streamable b => f b -> Stream b)
  -- The node to be evaluated:
  -> Node f a
  -- The resulting stream:
  -> Stream a
eval con n0 = case n0 of
  Const x         -> S.repeat x
  Append xs k     -> foldr (S.<:>) (con k) xs
  Drop i k        -> S.drop i (con k)
  -- Streams implement the applicative functor interface as zip's:
  Fun1 f k        -> fun1 f <$> con k
  Fun2 f k1 k2    -> fun2 f <$> con k1 <*> con k2
  Fun3 f k1 k2 k3 -> fun3 f <$> con k1 <*> con k2 <*> con k3
  Extern _        -> error "eval: Extern"

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
fun2 Index   = \ (Array xs) k -> xs !! (fromIntegral k)
fun2 Pow     = (**)
fun2 LogBase = logBase

fun3 :: Fun3 a b c d -> a -> b -> c -> d
fun3 Mux     = \ v x y -> if v then x else y
