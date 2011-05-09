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
  -- Streams implement the applicative functor interface by using zip's:
  Fun1 f k        -> fun1 f <$> con k
  Fun2 f k1 k2    -> fun2 f <$> con k1 <*> con k2
  Fun3 f k1 k2 k3 -> fun3 f <$> con k1 <*> con k2 <*> con k3
  Extern _        -> error "eval: Extern"

fun1
  :: (Streamable a, Streamable b)
  => Fun1 a b -> a -> b
fun1 Not = not
fun1 Abs = abs
fun1 Sgn = signum
fun1 Rcp = recip

fun2
  :: (Streamable a, Streamable b, Streamable c)
  => Fun2 a b c -> a -> b -> c
fun2 And = (&&)
fun2 Or  = (||)
fun2 Add = (+)
fun2 Sub = (-)
fun2 Mul = (*)
fun2 Div = (/)
fun2 Mod = mod
fun2 Eq  = (==)
fun2 Ne  = (/=)
fun2 Lt  = (<)
fun2 Gt  = (>)
fun2 Le  = (<=)
fun2 Ge  = (>=)
fun2 Idx = \ (Array xs) k -> xs !! (fromIntegral k)

fun3
  :: (Streamable a, Streamable b, Streamable c, Streamable d)  
  => Fun3 a b c d -> a -> b -> c -> d
fun3 Mux = \ v x y -> if v then x else y
