-- |

module Language.Copilot.Interface
  ( module Data.Int
  , module Data.Word
  , Stream
  , Streamable
  , interpret
  , (++)
  , (&&), (||)
  , (==), (/=)
  , (<=), (>=), (<), (>)
  , not
  , drop
  , const
  , mod
  , mux
  , true
  , false
  , extern
  , Array
  , array
  , (!!)
  ) where

import Data.Int
import Data.Word
import Language.Copilot.Core.Array (Array, newArray)
import Language.Copilot.Core.Node
import Language.Copilot.Core.Streamable
import Language.Copilot.Interface.Prelude
import Language.Copilot.Interface.Reify (reify)
import Language.Copilot.Interface.Stream (Stream (..), const, fun1, fun2, fun3)
import qualified Language.Copilot.Interpret as I
import qualified Prelude as P

interpret :: Streamable a => Integer -> Stream a -> IO ()
interpret i e =
  do
    sp <- reify e
    print (I.interpret i sp)

infixr 3 ++

(++) :: Streamable a => [a] -> Stream a -> Stream a
xs ++ (Stream a) = Stream . In $ Append xs a

not :: Stream Bool -> Stream Bool
not = fun1 Not

drop :: Streamable a => Int -> Stream a -> Stream a
drop k (Stream a) = Stream . In $ Drop k a

(&&) :: Stream Bool -> Stream Bool -> Stream Bool
(&&) = fun2 And

(||) :: Stream Bool -> Stream Bool -> Stream Bool
(||) = fun2 Or

(==) :: (Streamable a, P.Eq a) => Stream a -> Stream a -> Stream Bool
(==) = fun2 Eq

(/=) :: (Streamable a, P.Eq a) => Stream a -> Stream a -> Stream Bool
(/=) = fun2 Ne

(<=) :: (Streamable a, P.Ord a) => Stream a -> Stream a -> Stream Bool
(<=) = fun2 Le

(>=) :: (Streamable a, P.Ord a) => Stream a -> Stream a -> Stream Bool
(>=) = fun2 Ge

(<) :: (Streamable a, P.Ord a) => Stream a -> Stream a -> Stream Bool
(<) = fun2 Lt

(>) :: (Streamable a, P.Ord a) => Stream a -> Stream a -> Stream Bool
(>) = fun2 Gt

mod :: (Streamable a, Integral a) => Stream a -> Stream a -> Stream a
mod = fun2 Mod

mux :: Streamable a => Stream Bool -> Stream a -> Stream a -> Stream a
mux = fun3 Mux

true :: Stream Bool
true = const True

false :: Stream Bool
false = const False

extern :: Streamable a => String -> Stream a
extern = Stream . In . Extern

array :: Streamable a => [a] -> Stream (Array a)
array = Stream . In . Const . newArray

(!!) :: (Streamable a, Streamable i, Integral i)
  => Stream (Array a) -> Stream i -> Stream a
(!!) = fun2 Index
