--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Abstract syntax for streams and operators.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Language.Stream
  ( Stream (..) 
  , Arg (..) 
  ) where

import Copilot.Core (Typed, typeOf)
import Copilot.Core.Error
import qualified Copilot.Core as Core
import Copilot.Language.Prelude
import qualified Prelude as P

--------------------------------------------------------------------------------

data Stream :: * -> * where
  Append      :: Typed a 
              => [a] -> Maybe (Stream Bool) -> Stream a -> Stream a
  Const       :: Typed a => a -> Stream a
  Drop        :: Typed a
              => Int -> Stream a -> Stream a
  Extern      :: Typed a
              => String -> Maybe [a] -> Stream a
  ExternFun   :: Typed a 
              => String -> [Arg] -> Maybe (Stream a) -> Stream a
  ExternArray :: (Typed a, Typed b, Integral a)
              => String -> Stream a -> Int -> Maybe [[b]] -> Stream b
  Local       :: (Typed a, Typed b) 
              => Stream a -> (Stream a -> Stream b) -> Stream b
  Var         :: Typed a 
              => String -> Stream a
  Op1         :: (Typed a, Typed b)
              => Core.Op1 a b -> Stream a -> Stream b
  Op2         :: (Typed a, Typed b, Typed c)
              => Core.Op2 a b c -> Stream a -> Stream b -> Stream c
  Op3         :: (Typed a, Typed b, Typed c, Typed d)
              => Core.Op3 a b c d -> Stream a -> Stream b -> Stream c -> Stream d

--------------------------------------------------------------------------------

data Arg where
  Arg :: Typed a => Stream a -> Arg

--------------------------------------------------------------------------------

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance Show (Stream a) where
  show _      = "Stream"

--------------------------------------------------------------------------------

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance P.Eq (Stream a) where
  (==)        = badUsage "'Prelude.(==)' isn't implemented for streams!"
  (/=)        = badUsage "'Prelude.(/=)' isn't implemented for streams!"

--------------------------------------------------------------------------------

instance (Typed a, P.Eq a, Num a) => Num (Stream a) where
  (Const x) + (Const y)   = Const (x + y)
  (Const 0) + y           = y
  x + (Const 0)           = x
  x + y                   = Op2 (Core.Add typeOf) x y

  (Const x) - (Const y)   = Const (x - y)
  x - (Const 0)           = x
  x - y                   = Op2 (Core.Sub typeOf) x y

  (Const x) * (Const y)   = Const (x * y)
  (Const 0) * _           = Const 0
  _ * (Const 0)           = Const 0
  (Const 1) * y           = y
  x * (Const 1)           = x
  x * y                   = Op2 (Core.Mul typeOf) x y

  abs (Const x)           = Const (abs x)
  abs x                   = Op1 (Core.Abs typeOf) x

  signum (Const x)        = Const (signum x)
  signum x                = Op1 (Core.Sign typeOf) x

  fromInteger             = Const . fromInteger

--------------------------------------------------------------------------------

-- XXX we may not want to precompute these if they're constants if someone is
-- relying on certain floating-point behavior.
instance (Typed a, P.Eq a, Fractional a) => Fractional (Stream a) where
  (/)                     = Op2 (Core.Fdiv typeOf) 

  recip (Const x)         = Const (recip x)
  recip x                 = Op1 (Core.Recip typeOf) x

  fromRational            = Const . fromRational

--------------------------------------------------------------------------------

-- XXX we may not want to precompute these if they're constants if someone is
-- relying on certain floating-point behavior.
instance (Typed a, Eq a, Floating a) => Floating (Stream a) where
  pi           = Const pi
  exp          = Op1 (Core.Exp typeOf)
  sqrt         = Op1 (Core.Sqrt typeOf)
  log          = Op1 (Core.Log typeOf)
  (**)         = Op2 (Core.Pow typeOf)
  logBase      = Op2 (Core.Logb typeOf)
  sin          = Op1 (Core.Sin typeOf)
  tan          = Op1 (Core.Tan typeOf)
  cos          = Op1 (Core.Cos typeOf)
  asin         = Op1 (Core.Asin typeOf)
  atan         = Op1 (Core.Atan typeOf)
  acos         = Op1 (Core.Acos typeOf)
  sinh         = Op1 (Core.Sinh typeOf)
  tanh         = Op1 (Core.Tanh typeOf)
  cosh         = Op1 (Core.Cosh typeOf)
  asinh        = Op1 (Core.Asinh typeOf)
  atanh        = Op1 (Core.Atanh typeOf)
  acosh        = Op1 (Core.Acosh typeOf)

--------------------------------------------------------------------------------
