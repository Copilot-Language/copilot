-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Language.Copilot.Interface.Stream
  ( Stream (..)
  ) where

import Language.Copilot.Core (Fun1 (..), Fun2 (..), Fun3 (..), Streamable)

data Stream :: * -> * where
  Append
    :: Streamable a
    => [a]
    -> Stream a
    -> Stream a
  Const
    :: Streamable a
    => a
    -> Stream a
  Drop
    :: Streamable a
    => Int
    -> Stream a
    -> Stream a
  Extern
    :: Streamable a
    => String
    -> Stream a
  Fun1
    :: (Streamable a, Streamable b)
    => Fun1 a b
    -> Stream a
    -> Stream b
  Fun2
    :: (Streamable a, Streamable b, Streamable c)
    => Fun2 a b c
    -> Stream a
    -> Stream b
    -> Stream c
  Fun3
    :: (Streamable a, Streamable b, Streamable c, Streamable d)
    => Fun3 a b c d
    -> Stream a
    -> Stream b
    -> Stream c
    -> Stream d

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance Show (Stream a) where
  show _ = error "'Prelude.show' isn't implemented for streams!"

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance Eq (Stream a) where
  (==) = error "'Prelude.(==)' isn't implemented for streams!"
  (/=) = error "'Prelude.(/=)' isn't implemented for streams!"

instance (Streamable a, Num a) => Num (Stream a) where
  (+)     = Fun2 Add
  (-)     = Fun2 Sub
  (*)     = Fun2 Mul
  abs     = Fun1 Abs
  signum  = Fun1 Signum
  fromInteger = Const . fromInteger

instance (Streamable a, Fractional a) => Fractional (Stream a) where
  (/)     = Fun2 Div
  recip   = Fun1 Recip
  fromRational = Const . fromRational

instance (Streamable a, Floating a) => Floating (Stream a) where
  pi      = Const pi
  exp     = Fun1 Exp
  sqrt    = Fun1 Sqrt
  log     = Fun1 Log
  (**)    = Fun2 Pow
  logBase = Fun2 LogBase
  sin     = Fun1 Sin
  tan     = Fun1 Tan
  cos     = Fun1 Cos
  asin    = Fun1 Asin
  atan    = Fun1 Atan
  acos    = Fun1 Acos
  sinh    = Fun1 Sinh
  tanh    = Fun1 Tanh
  cosh    = Fun1 Cosh
  asinh   = Fun1 Asinh
  atanh   = Fun1 Atanh
  acosh   = Fun1 Acosh
