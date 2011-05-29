-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Copilot.Language.Stream
  ( Stream (..)
  , Trigger (..)
  , trigger
  , constant
  ) where

import Copilot.Core (Streamable)
import qualified Copilot.Core as Core
import Copilot.Language.Operators.Boolean
import Copilot.Language.Operators.Eq
import Copilot.Language.Operators.Extern
import Copilot.Language.Operators.Mux
import Copilot.Language.Operators.Ord
import Copilot.Language.Operators.Temporal
import Copilot.Language.Prelude
import qualified Prelude as P

data Stream ∷ * → * where
  Append
    ∷ Streamable α
    ⇒ [α]
    → Maybe (Stream Bool)
    → Stream α
    → Stream α
  Const
    ∷ Streamable α
    ⇒ α
    → Stream α
  Drop
    ∷ Streamable α
    ⇒ Int
    → Stream α
    → Stream α
  Extern
    ∷ Streamable α
    ⇒ String
    → Stream α
  Op1
    ∷ (Streamable α, Streamable β)
    ⇒ (∀ θ . Core.Op1 θ => θ α β)
    → Stream α
    → Stream β
  Op2
    ∷ (Streamable α, Streamable β, Streamable γ)
    ⇒ (∀ θ . Core.Op2 θ => θ α β γ)
    → Stream α
    → Stream β
    → Stream γ
  Op3
    ∷ (Streamable α, Streamable β, Streamable γ, Streamable δ)
    ⇒ (∀ θ . Core.Op3 θ => θ α β γ δ)
    → Stream α
    → Stream β
    → Stream γ
    → Stream δ

data Trigger where
  Trigger
    ∷ Streamable α
    ⇒ Core.Name
    → Stream Bool
    → Stream α
    → Trigger

trigger
  ∷ Streamable α
  ⇒ String
  → Stream Bool
  → Stream α
  → Trigger
trigger = Trigger

constant ∷ Streamable α ⇒ α → Stream α
constant = Const

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance Show (Stream α) where
  show _      = "Stream"

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance P.Eq (Stream α) where
  (==)        = error "'Prelude.(==)' isn't implemented for streams!"
  (/=)        = error "'Prelude.(/=)' isn't implemented for streams!"

--
-- Unfortunately we can't instantiate boolean streams like this:
--
--   instance (Streamable α, Boolean α) => Boolean (Stream α) where
--     (&&)        = Op2 (Core.&&.)
--     (||)        = Op2 (Core.||.)
--     not         = Op1 Core.not'
--     true        = Const true
--     false       = Const false
--     fromBool    = Const . fromBool
--
-- ...as we would be required to use the 'Boolean' class in the
-- core-representation by doing so.
--
instance Boolean (Stream Bool) where
  (&&)        = Op2 (Core.and)
  (||)        = Op2 (Core.or)
  not         = Op1 Core.not
  true        = Const true
  false       = Const false
  fromBool    = Const . fromBool

instance Streamable α ⇒ Mux (Stream α) (Stream Bool) where
  mux         = Op3 (Core.mux)

instance (Streamable α, Num α) ⇒ Num (Stream α) where
  (+)         = Op2 (Core.add)
  (-)         = Op2 (Core.sub)
  (*)         = Op2 (Core.mul)
  abs         = Op1 Core.abs
  signum      = Op1 Core.sign
  fromInteger = Const . fromInteger

instance (Streamable α, P.Eq α) ⇒ Eq (Stream α) (Stream Bool) where
  (==)        = Op2 (Core.eq)
  (/=)        = Op2 (Core.ne)

instance (Streamable α, P.Ord α) ⇒ Ord (Stream α) (Stream Bool) where
  (<=)        = Op2 (Core.le)
  (>=)        = Op2 (Core.ge)
  (<)         = Op2 (Core.lt)
  (>)         = Op2 (Core.gt)

instance Streamable β ⇒ Temporal Stream β where
  (++)        = (`Append` Nothing)
  drop        = Drop 

instance Extern Stream where
  extern      = Extern

{-
instance (Streamable α, Fractional α) ⇒ Fractional (Stream α) where
  (/)     = Op2 (:/:)
  recip   = Op1 Recip
  fromRational = Const . fromRational
-}

{-
instance (Streamable α, Floating α) ⇒ Floating (Stream α) where
  pi      = Const pi
  exp     = Op1 Exp
  sqrt    = Op1 Sqrt
  log     = Op1 Log
  (**)    = Op2 Pow
  logBase = Op2 LogBase
  sin     = Op1 Sin
  tan     = Op1 Tan
  cos     = Op1 Cos
  asin    = Op1 Asin
  atan    = Op1 Atan
  acos    = Op1 Acos
  sinh    = Op1 Sinh
  tanh    = Op1 Tanh
  cosh    = Op1 Cosh
  asinh   = Op1 Asinh
  atanh   = Op1 Atanh
  acosh   = Op1 Acosh
-}
