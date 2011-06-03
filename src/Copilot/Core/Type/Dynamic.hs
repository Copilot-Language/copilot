--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | An implementation of dynamic types using "Copilot.Core.Type.Equality".
-- The theory behind this technique is described the following paper:
--
-- * Baars, Arthur I. and Swierstra, S. Doaitse,
-- \"/Typing dynamic typing/\",
-- ACM SIGPLAN Notices vol. 37, p. 157-166, 2002

{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Core.Type.Dynamic
  ( Dynamic
  , DynamicF
  , toDynamic
  , fromDynamic
  , toDynamicF
  , fromDynamicF
  ) where

import Copilot.Core.Type.Equality

--------------------------------------------------------------------------------

data Dynamic τ = forall a . Dynamic a (τ a)

toDynamic :: a -> τ a -> Dynamic τ
toDynamic = Dynamic

fromDynamic :: EqualType τ => τ a -> Dynamic τ -> Maybe a
fromDynamic t2 (Dynamic x t1) =
  case t1 =~= t2 of
    Just eq -> Just (coerce eq x)
    Nothing -> Nothing

--------------------------------------------------------------------------------

data DynamicF f τ = forall a . DynamicF (f a) (τ a)

toDynamicF :: f a -> τ a -> DynamicF f τ
toDynamicF = DynamicF

fromDynamicF :: EqualType τ => τ a -> DynamicF f τ -> Maybe (f a)
fromDynamicF t2 (DynamicF fx t1) =
  case t1 =~= t2 of
    Just eq -> Just (coerce (cong eq) fx)
    Nothing -> Nothing

--------------------------------------------------------------------------------