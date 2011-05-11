-- |

module Language.Copilot.Interface
  ( module Data.Int
  , module Data.Word
  , Stream
  , Streamable
  , Spec
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
  , interpret
  ) where

import Data.Int
import Data.Word
import Language.Copilot.Core
  ( Node (..), Mu2 (..)
  , Fun1 (..), Fun2 (..), Fun3 (..)
  , Streamable, Spec (..)
  )
import Language.Copilot.Core.Array (Array (..))
import Language.Copilot.Interface.Prelude
import Language.Copilot.Interface.Reify (reify)
import qualified Language.Copilot.Interpret as I
import qualified Prelude as P

-- | A wrapper for expressions
newtype Stream a = Stream (Mu2 Node a)

fun1 :: (Streamable a, Streamable b)
  => Fun1 a b
  -> Stream a
  -> Stream b
fun1 f (Stream x) = Stream . In $ Fun1 f x

fun2 :: (Streamable a, Streamable b, Streamable c)
  => Fun2 a b c
  -> Stream a
  -> Stream b
  -> Stream c
fun2 f (Stream a) (Stream b) = Stream . In $ Fun2 f a b

fun3 :: (Streamable a, Streamable b, Streamable c, Streamable d)
  => Fun3 a b c d
  -> Stream a
  -> Stream b
  -> Stream c
  -> Stream d
fun3 f (Stream a) (Stream b) (Stream c) = Stream . In $ Fun3 f a b c

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance Show (Stream a) where
  show _ = error "'Prelude.show' isn't implemented for streams!"

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance P.Eq (Stream a) where
  (==) = error "'Prelude.(==)' isn't implemented for streams!"
  (/=) = error "'Prelude.(/=)' isn't implemented for streams!"

instance (Streamable a, Num a) => Num (Stream a) where
  (+)     = fun2 Add
  (-)     = fun2 Sub
  (*)     = fun2 Mul
  abs     = fun1 Abs
  signum  = fun1 Signum
  fromInteger = const . fromInteger

instance (Streamable a, Fractional a) => Fractional (Stream a) where
  (/)     = fun2 Div
  recip   = fun1 Recip
  fromRational = const . fromRational

instance (Streamable a, Floating a) => Floating (Stream a) where
  pi      = const pi
  exp     = fun1 Exp
  sqrt    = fun1 Sqrt
  log     = fun1 Log
  (**)    = fun2 Pow
  logBase = fun2 LogBase
  sin     = fun1 Sin
  tan     = fun1 Tan
  cos     = fun1 Cos
  asin    = fun1 Asin
  atan    = fun1 Atan
  acos    = fun1 Acos
  sinh    = fun1 Sinh
  tanh    = fun1 Tanh
  cosh    = fun1 Cosh
  asinh   = fun1 Asinh
  atanh   = fun1 Atanh
  acosh   = fun1 Acosh

infixr 3 ++

(++) :: Streamable a => [a] -> Stream a -> Stream a
xs ++ (Stream a) = Stream . In $ Append xs a

not :: Stream Bool -> Stream Bool
not = fun1 Not

drop :: Streamable a => Int -> Stream a -> Stream a
drop k (Stream a) = Stream . In $ Drop k a

const :: Streamable a => a -> Stream a
const = Stream . In . Const

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
array = Stream . In . Const . Array

(!!) :: (Streamable a, Streamable i, Integral i)
  => Stream (Array a) -> Stream i -> Stream a
(!!) = fun2 Index

interpret :: Integer -> Streamable a => Stream a -> IO ()
interpret i (Stream n) =
  do
    (m, k) <- reify n
    print $ I.interpret i (Spec m k)
