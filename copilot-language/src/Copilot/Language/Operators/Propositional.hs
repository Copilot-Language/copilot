-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}

-- | Implement negation over quantified extensions of boolean streams.
--
-- For details, see 'Prop'.
module Copilot.Language.Operators.Propositional (not) where

import Prelude (($))

import Copilot.Language.Spec (Prop (..))
import qualified Copilot.Language.Operators.Boolean as B

import Copilot.Theorem

-- | A proposition that can be negated.
class Negatable a b where
  -- | Negate a proposition.
  not :: a -> b

-- | Negation of an existentially quantified proposition.
instance Negatable (Prop Existential) (Prop Universal) where
  not (Exists p)  = Forall $ B.not p

-- | Negation of a universally quantified proposition.
instance Negatable (Prop Universal) (Prop Existential) where
  not (Forall p)  = Exists $ B.not p
