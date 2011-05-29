-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

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
  ) where

import Copilot.Core.Type.Equality

{-
data Dynamic = forall α . Dynamic α (Type α)

data DynamicF f = forall α . DynamicF (f α) (Type α)

toDynamic :: Typed α => α -> Dynamic
toDynamic x = Dynamic x typeOf

fromDynamic :: Typed α => Dynamic -> Maybe α
fromDynamic (Dynamic x t) =
  case t =~= typeOf of
    Just eq -> Just (coerce eq x)
    Nothing -> Nothing

toDynamicF :: Typed α => f α -> DynamicF f
toDynamicF x = DynamicF x typeOf

fromDynamicF :: Typed α => DynamicF f -> Maybe (f α)
fromDynamicF (DynamicF fx t) =
  case t =~= typeOf of
    Just eq -> Just (coerce (cong eq) fx)
    Nothing -> Nothing
-}

data Dynamic τ = forall α . Dynamic α (τ α)

data DynamicF f τ = forall α . DynamicF (f α) (τ α)

toDynamic :: α -> τ α -> Dynamic τ
toDynamic = Dynamic

fromDynamic :: EqualType τ => τ α -> Dynamic τ -> Maybe α
fromDynamic t2 (Dynamic x t1) =
  case t1 =~= t2 of
    Just eq -> Just (coerce eq x)
    Nothing -> Nothing

toDynamicF :: f α -> τ α -> DynamicF f τ
toDynamicF = DynamicF

fromDynamicF :: EqualType τ => τ α -> DynamicF f τ -> Maybe (f α)
fromDynamicF t2 (DynamicF fx t1) =
  case t1 =~= t2 of
    Just eq -> Just (coerce (cong eq) fx)
    Nothing -> Nothing
