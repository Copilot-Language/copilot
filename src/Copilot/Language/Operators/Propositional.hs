--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Trustworthy, FlexibleInstances, GADTs, MultiParamTypeClasses #-}

module Copilot.Language.Operators.Propositional (not) where

import Prelude (($))

import Copilot.Language.Spec (Prop (..))
import qualified Copilot.Language.Operators.Boolean as B

import Copilot.Theorem

--------------------------------------------------------------------------------

class Negatable a b where
  not :: a -> b

instance Negatable (Prop Existential) (Prop Universal) where
  not (Exists p)  = Forall $ B.not p

instance Negatable (Prop Universal) (Prop Existential) where
  not (Forall p)  = Exists $ B.not p

--------------------------------------------------------------------------------
