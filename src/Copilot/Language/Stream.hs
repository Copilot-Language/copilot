--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}

module Copilot.Language.Stream
  ( Stream (..)
  , Trigger (..)
  , TriggerArg (..)
  , trigger
  , triggerArg
  , constant
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Operators.Boolean
import Copilot.Language.Operators.Eq
import Copilot.Language.Operators.Extern
import Copilot.Language.Operators.Integral
import Copilot.Language.Operators.Mux
import Copilot.Language.Operators.Ord
import Copilot.Language.Operators.Temporal
import Copilot.Language.Prelude
import Data.Word (Word8)
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
    => Word8
    -> Stream a
    -> Stream a
  Extern
    :: Typed a
    => String
    -> Stream a
  Op1
    :: (Typed a, Typed b)
    => (forall op . Core.Op1 op => op a b)
    -> Stream a
    -> Stream b
  Op2
    :: (Typed a, Typed b, Typed c)
    => (forall op . Core.Op2 op => op a b c)
    -> Stream a
    -> Stream b
    -> Stream c
  Op3
    :: (Typed a, Typed b, Typed c, Typed d)
    => (forall op . Core.Op3 op => op a b c d)
    -> Stream a
    -> Stream b
    -> Stream c
    -> Stream d

--------------------------------------------------------------------------------

data Trigger where
  Trigger
    :: Core.Name
    -> Stream Bool
    -> [TriggerArg]
    -> Trigger

data TriggerArg where
  TriggerArg
    :: Typed a
    => Stream a
    -> TriggerArg

trigger
  :: String
  -> Stream Bool
  -> [TriggerArg]
  -> Trigger
trigger = Trigger

triggerArg :: Typed a => Stream a -> TriggerArg
triggerArg = TriggerArg

--------------------------------------------------------------------------------

constant :: Typed a => a -> Stream a
constant = Const

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

-- | Unfortunately we can't instantiate boolean streams like this:
-- 
-- @
--   instance (Streamable a, Boolean a) => Boolean (Stream a) where
--     (&&)        = Op2 (Core.&&.)
--     (||)        = Op2 (Core.||.)
--     not         = Op1 Core.not'
--     true        = Const true
--     false       = Const false
--     fromBool    = Const . fromBool
-- @
--
-- ...as we would be required to use the 'Boolean' class in the
-- core-representation by doing so.
instance Boolean (Stream Bool) where
  (&&)        = Op2 Core.and
  (||)        = Op2 Core.or
  not         = Op1 Core.not
  true        = Const true
  false       = Const false
  fromBool    = Const . fromBool

--------------------------------------------------------------------------------

instance Typed a => Mux (Stream a) (Stream Bool) where
  mux         = Op3 (Core.mux typeOf)

--------------------------------------------------------------------------------

instance (Typed a, Num a) => Num (Stream a) where
  (+)         = Op2 (Core.add typeOf)
  (-)         = Op2 (Core.sub typeOf)
  (*)         = Op2 (Core.mul typeOf)
  abs         = Op1 (Core.abs typeOf)
  signum      = Op1 (Core.sign typeOf)
  fromInteger = Const . fromInteger

--------------------------------------------------------------------------------

instance (Typed a, P.Integral a) => Integral (Stream a) where
  div         = Op2 (Core.div typeOf)
  mod         = Op2 (Core.mod typeOf)

--------------------------------------------------------------------------------

instance (Typed a, P.Eq a) => Eq (Stream a) (Stream Bool) where
  (==)        = Op2 (Core.eq typeOf)
  (/=)        = Op2 (Core.ne typeOf)

--------------------------------------------------------------------------------

instance (Typed a, P.Ord a) => Ord (Stream a) (Stream Bool) where
  (<=)        = Op2 (Core.le typeOf)
  (>=)        = Op2 (Core.ge typeOf)
  (<)         = Op2 (Core.lt typeOf)
  (>)         = Op2 (Core.gt typeOf)

--------------------------------------------------------------------------------

instance Typed b => Temporal Stream b where
  (++)        = (`Append` Nothing)
  drop i      = Drop (fromIntegral i)

--------------------------------------------------------------------------------

instance Extern Stream where
  extern      = Extern

--------------------------------------------------------------------------------

instance (Typed a, Fractional a) => Fractional (Stream a) where
  (/)          = Op2 (Core.fdiv typeOf)
  recip        = Op1 (Core.recip typeOf)
  fromRational = Const . fromRational

--------------------------------------------------------------------------------

{-
instance (Streamable a, Floating a) => Floating (Stream a) where
  pi           = Const pi
  exp          = Op1 Core.exp
  sqrt         = Op1 Core.sqrt
  log          = Op1 Core.log
  (**)         = Op2 Core.pow
  logBase      = Op2 Core.logb
  sin          = Op1 Core.sin
  tan          = Op1 Core.tan
  cos          = Op1 Core.cos
  asin         = Op1 Core.asin
  atan         = Op1 Core.atan
  acos         = Op1 Core.acos
  sinh         = Op1 Core.sinh
  tanh         = Op1 Core.tanh
  cosh         = Op1 Core.cosh
  asinh        = Op1 Core.asinh
  atanh        = Op1 Core.atanh
  acosh        = Op1 Core.acosh
-}

--------------------------------------------------------------------------------