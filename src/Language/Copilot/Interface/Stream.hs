-- |

module Language.Copilot.Interface.Stream
  ( Stream (..)
  , const
  , fun1
  , fun2
  , fun3
  ) where

import Language.Copilot.Core
  ( Node (..), Mu2 (..)
  , Fun1 (..), Fun2 (..), Fun3 (..)
  , Streamable
  )
import Language.Copilot.Interface.Prelude
import qualified Prelude as P

newtype Stream a = Stream (Mu2 Node a)

const :: Streamable a => a -> Stream a
const = Stream . In . Const

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
