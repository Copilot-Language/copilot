--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Language.Stream
  ( Stream (..)
  , FunArg (..)
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Prelude
--import Data.Word (Word8)
import qualified Prelude as P

--------------------------------------------------------------------------------

data Stream :: * -> * where
  Append
    :: Typed a
    => [a]
    -> Maybe (Stream Bool)
    -> Stream a
    -> Stream a
  Const
    :: Typed a
    => a
    -> Stream a
  Drop
    :: Typed a
    => Int
    -> Stream a
    -> Stream a
  Extern
    :: Typed a
    => String
    -> Stream a
  ExternFun
    :: Typed a
    => String
    -> [FunArg]
    -> Stream a
  ExternArray
    :: (Typed a, Typed b, Integral a)
    => String
    -> Stream a
    -> Stream b
  Local
    :: (Typed a, Typed b)
    => Stream a
    -> (Stream a -> Stream b)
    -> Stream b
  Var
    :: Typed a
    => String
    -> Stream a
  Op1
    :: (Typed a, Typed b)
    => (forall op . Core.Op1 op => op a b)
    -> Stream a -> Stream b
  Op2
    :: (Typed a, Typed b, Typed c)
    => (forall op . Core.Op2 op => op a b c)
    -> Stream a -> Stream b -> Stream c
  Op3
    :: (Typed a, Typed b, Typed c, Typed d)
    => (forall op . Core.Op3 op => op a b c d)
    -> Stream a
    -> Stream b
    -> Stream c
    -> Stream d

--------------------------------------------------------------------------------

data FunArg where
  FunArg :: Typed a => Stream a -> FunArg

--------------------------------------------------------------------------------

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance Show (Stream a) where
  show _      = "Stream"

--------------------------------------------------------------------------------

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance P.Eq (Stream a) where
  (==)        = error "'Prelude.(==)' isn't implemented for streams!"
  (/=)        = error "'Prelude.(/=)' isn't implemented for streams!"

--------------------------------------------------------------------------------

instance (Typed a, Num a) => Num (Stream a) where
  (Const x) + (Const y)   = Const (x + y)
  (Const 0) + y           = y
  x + (Const 0)           = x
  x + y                   = Op2 (Core.add typeOf) x y

  (Const x) - (Const y)   = Const (x - y)
  x - (Const 0)           = x
  x - y                   = Op2 (Core.sub typeOf) x y

  (Const x) * (Const y)   = Const (x * y)
  (Const 0) * _           = Const 0
  _ * (Const 0)           = Const 0
  (Const 1) * y           = y
  x * (Const 1)           = x
  x * y                   = Op2 (Core.mul typeOf) x y

  abs (Const x)           = Const (abs x)
  abs x                   = Op1 (Core.abs typeOf) x

  signum (Const x)        = Const (signum x)
  signum x                = Op1 (Core.sign typeOf) x

  fromInteger             = Const . fromInteger

--------------------------------------------------------------------------------

-- XXX we may not want to precompute these if they're constants if someone is
-- relying on certain floating-point behavior.
instance (Typed a, Fractional a) => Fractional (Stream a) where
  (/)                     = Op2 (Core.fdiv typeOf) 

  recip (Const x)         = Const (recip x)
  recip x                 = Op1 (Core.recip typeOf) x

  fromRational            = Const . fromRational

--------------------------------------------------------------------------------

-- XXX we may not want to precompute these if they're constants if someone is
-- relying on certain floating-point behavior.
instance (Typed a, Floating a) => Floating (Stream a) where
  pi           = Const pi
  exp          = Op1 (Core.exp typeOf)
  sqrt         = Op1 (Core.sqrt typeOf)
  log          = Op1 (Core.log typeOf)
  (**)         = Op2 (Core.pow typeOf)
  logBase      = Op2 (Core.logb typeOf)
  sin          = Op1 (Core.sin typeOf)
  tan          = Op1 (Core.tan typeOf)
  cos          = Op1 (Core.cos typeOf)
  asin         = Op1 (Core.asin typeOf)
  atan         = Op1 (Core.atan typeOf)
  acos         = Op1 (Core.acos typeOf)
  sinh         = Op1 (Core.sinh typeOf)
  tanh         = Op1 (Core.tanh typeOf)
  cosh         = Op1 (Core.cosh typeOf)
  asinh        = Op1 (Core.asinh typeOf)
  atanh        = Op1 (Core.atanh typeOf)
  acosh        = Op1 (Core.acosh typeOf)

--------------------------------------------------------------------------------

