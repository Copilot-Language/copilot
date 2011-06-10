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
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Type.Dynamic
  ( Dynamic
  , DynamicF
  , toDynamic
  , fromDynamic
  , toDynamicF
  , fromDynamicF
  , mapDynamicF
  ) where

import Copilot.Core.Type.Equality

--------------------------------------------------------------------------------

data Dynamic t = forall a . Dynamic a (t a)

toDynamic :: a -> t a -> Dynamic t
toDynamic = Dynamic

fromDynamic :: EqualType t => t a -> Dynamic t -> Maybe a
fromDynamic t2 (Dynamic x t1) =
  case t1 =~= t2 of
    Just eq -> Just (coerce eq x)
    Nothing -> Nothing

--------------------------------------------------------------------------------

data DynamicF f t = forall a . DynamicF (f a) (t a)

toDynamicF :: f a -> t a -> DynamicF f t
toDynamicF = DynamicF

fromDynamicF :: EqualType t => t a -> DynamicF f t -> Maybe (f a)
fromDynamicF t2 (DynamicF fx t1) =
  case t1 =~= t2 of
    Just eq -> Just (coerce (cong eq) fx)
    Nothing -> Nothing

mapDynamicF
  :: EqualType t
  => (forall a . f a -> g a)
  -> DynamicF f t
  -> DynamicF g t
mapDynamicF f (DynamicF fx t) = DynamicF (f fx) t

--------------------------------------------------------------------------------