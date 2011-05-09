-- |

{-# LANGUAGE GADTs #-}

module Language.Copilot.Interpret
  ( interpret
  ) where

import Language.Copilot.Node (Node (..), Fun1 (..), Fun2 (..), Fun3 (..))
import Language.Copilot.Array (Array (..))
import Language.Copilot.Spec (Spec (..), Map2, Key, lookup, fmap2)
import Language.Copilot.Streamable (Streamable)
import Prelude hiding (lookup)

interpret :: Streamable a => Spec a -> [a]
interpret (Spec m k0) =
    eval env (lookup k0 m)
  where
    env = fmap2 (eval env) m

eval :: Map2 x [] -> Node (Key x) a -> [a]
eval m e0 = case e0 of
  Const x         -> repeat x
  Append xs k     -> xs ++ (lookup k m)
  Drop i k        -> drop i (lookup k m)
  Fun1 f k1       -> fmap (fun1 f)
                       (lookup k1 m)
  Fun2 f k1 k2    -> zipWith (fun2 f)
                       (lookup k1 m)
                       (lookup k2 m)
  Fun3 f k1 k2 k3 -> zipWith3 (fun3 f)
                       (lookup k1 m)
                       (lookup k2 m)
                       (lookup k3 m)
  Extern _        -> error "eval: Extern"

fun1
  :: (Streamable a, Streamable b)
  => Fun1 a b
  -> a
  -> b
fun1 Not = not
fun1 Abs = abs
fun1 Sgn = signum
fun1 Rcp = recip

fun2
  :: (Streamable a, Streamable b, Streamable c)
  => Fun2 a b c
  -> a
  -> b
  -> c
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
  => Fun3 a b c d
  -> a
  -> b
  -> c
  -> d
fun3 Mux = \ v x y -> if v then x else y
