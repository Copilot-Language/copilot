-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Interface.Stream
  ( Stream (..)
  , constant
  ) where

import Language.Copilot.Core (Streamable)
import qualified Language.Copilot.Core as Core
import Language.Copilot.Interface.Operators.Boolean
import Language.Copilot.Interface.Operators.Eq
import Language.Copilot.Interface.Operators.Extern
import Language.Copilot.Interface.Operators.Mux
import Language.Copilot.Interface.Operators.Ord
import Language.Copilot.Interface.Operators.Temporal
import Language.Copilot.Interface.Prelude
import qualified Prelude as P

data Stream ∷ * → * where
  Append
    ∷ Streamable α
    ⇒ [α]
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

constant ∷ Streamable α ⇒ α → Stream α
constant = Const

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance Show (Stream α) where
  show _      = "Stream"

-- | Dummy instance in order to make 'Stream' an instance of 'Num'.
instance P.Eq (Stream α) where
  (==)        = error "'Prelude.(==)' isn't implemented for streams!"
  (/=)        = error "'Prelude.(/=)' isn't implemented for streams!"

{-
instance (Streamable α, Boolean α) => Boolean (Stream α) where
  (&&)        = Op2 (Core.&&.)
  (||)        = Op2 (Core.||.)
  not         = Op1 Core.not'
  true        = Const true
  false       = Const false
  fromBool    = Const . fromBool
-}

instance Boolean (Stream Bool) where
  (&&)        = Op2 (Core.&&.)
  (||)        = Op2 (Core.||.)
  not         = Op1 Core.not'
  true        = Const true
  false       = Const false
  fromBool    = Const . fromBool

instance Streamable α ⇒ Mux (Stream Bool) (Stream α) where
  mux         = Op3 (Core.if_then_else)

instance (Streamable α, Num α) ⇒ Num (Stream α) where
  (+)         = Op2 (Core.+.)
  (-)         = Op2 (Core.-.)
  (*)         = Op2 (Core.*.)
  abs         = Op1 Core.abs'
  signum      = Op1 Core.signum'
  fromInteger = Const . fromInteger

instance (Streamable α, P.Eq α) ⇒ Eq (Stream Bool) (Stream α) where
  (==)        = Op2 (Core.==.)
  (/=)        = Op2 (Core./=.)

instance (Streamable α, P.Ord α) ⇒ Ord (Stream Bool) (Stream α) where
  (<=)        = Op2 (Core.<=.)
  (>=)        = Op2 (Core.>=.)
  (<)         = Op2 (Core.<.)
  (>)         = Op2 (Core.>.)

instance Streamable β ⇒ Temporal β Stream where
  (++)        = Append
  drop        = Drop 

instance Streamable β ⇒ Extern β Stream where
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
