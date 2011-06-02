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
  , stream
  , constant
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Operators.Boolean
import Copilot.Language.Operators.Eq
import Copilot.Language.Operators.Extern
import Copilot.Language.Operators.Mux
import Copilot.Language.Operators.Ord
import Copilot.Language.Operators.Temporal
import Copilot.Language.Prelude
import Data.Word (Word8)
import qualified Prelude as P

--------------------------------------------------------------------------------

data Stream :: * -> * where
  Append
    :: (Show α, Typed α)
    => [α]
    -> Maybe (Stream Bool)
    -> Stream α
    -> Stream α
  Const
    :: (Show α, Typed α)
    => α
    -> Stream α
  Drop
    :: (Show α, Typed α)
    => Word8
    -> Stream α
    -> Stream α
  Extern
    :: (Show α, Typed α)
    => String
    -> Stream α
  Op1
    :: (Typed α, Typed β, Show α, Show β)
    => (forall θ . Core.Op1 θ => θ α β)
    -> Stream α
    -> Stream β
  Op2
    :: (Typed α, Typed β, Typed γ, Show α, Show β, Show γ)
    => (forall θ . Core.Op2 θ => θ α β γ)
    -> Stream α
    -> Stream β
    -> Stream γ
  Op3
    :: (Typed α, Typed β, Typed γ, Typed δ, Show α, Show β, Show γ, Show δ)
    => (forall θ . Core.Op3 θ => θ α β γ δ)
    -> Stream α
    -> Stream β
    -> Stream γ
    -> Stream δ

--------------------------------------------------------------------------------

data Trigger where
  Trigger
    :: Core.Name
    -> Stream Bool
    -> [TriggerArg]
    -> Trigger

data TriggerArg where
  TriggerArg
    :: (Show α, Typed α)
    => Stream α
    -> TriggerArg

trigger
  :: String
  -> Stream Bool
  -> [TriggerArg]
  -> Trigger
trigger = Trigger

stream :: (Show α, Typed α) => Stream α -> TriggerArg
stream = TriggerArg

--------------------------------------------------------------------------------

constant :: (Show α, Typed α) => α -> Stream α
constant = Const

--------------------------------------------------------------------------------

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance Show (Stream α) where
  show _      = "Stream"

--------------------------------------------------------------------------------

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance P.Eq (Stream α) where
  (==)        = error "'Prelude.(==)' isn't implemented for streams!"
  (/=)        = error "'Prelude.(/=)' isn't implemented for streams!"

--------------------------------------------------------------------------------

-- | Unfortunately we can't instantiate boolean streams like this:
-- 
-- @
--   instance (Streamable α, Boolean α) => Boolean (Stream α) where
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

instance (Typed α, Show α) => Mux (Stream α) (Stream Bool) where
  mux         = Op3 (Core.mux typeOf)

--------------------------------------------------------------------------------

instance (Typed α, Num α) => Num (Stream α) where
  (+)         = Op2 (Core.add typeOf)
  (-)         = Op2 (Core.sub typeOf)
  (*)         = Op2 (Core.mul typeOf)
  abs         = Op1 (Core.abs typeOf)
  signum      = Op1 (Core.sign typeOf)
  fromInteger = Const . fromInteger

--------------------------------------------------------------------------------

instance (Typed α, P.Eq α, Show α) => Eq (Stream α) (Stream Bool) where
  (==)        = Op2 (Core.eq typeOf)
  (/=)        = Op2 (Core.ne typeOf)

--------------------------------------------------------------------------------

instance (Typed α, P.Ord α, Show α) => Ord (Stream α) (Stream Bool) where
  (<=)        = Op2 (Core.le typeOf)
  (>=)        = Op2 (Core.ge typeOf)
  (<)         = Op2 (Core.lt typeOf)
  (>)         = Op2 (Core.gt typeOf)

--------------------------------------------------------------------------------

instance (Typed β, Show β) => Temporal Stream β where
  (++)        = (`Append` Nothing)
  drop i      = Drop (fromIntegral i)

--------------------------------------------------------------------------------

instance Extern Stream where
  extern      = Extern

--------------------------------------------------------------------------------

{-
instance (Streamable α, Fractional α) => Fractional (Stream α) where
  (/)          = Op2 Core.div
  recip        = Op1 Core.recip
  fromRational = Const . fromRational
-}

--------------------------------------------------------------------------------

{-
instance (Streamable α, Floating α) => Floating (Stream α) where
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