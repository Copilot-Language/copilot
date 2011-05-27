-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- |

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Interface.DynMap where
--  ( DynMap
--  , DynKey (..)
--  , empty
--  , insert
--  ) where

{-
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Type.Equality (eqT, coerce, cong)
import qualified Data.Traversable as T
import Language.Copilot.Core (HasType (..))
import Language.Copilot.Core.HeteroMap (HeteroMap (..), Key, Key2Int (..))

data Dyn f = ∀ α . HasType α ⇒ Dyn (f α)

newtype DynMap f = DynMap (IntMap (Dyn f))

newtype DynKey α = DynKey Int
  deriving (Eq, Ord, Show)

instance HeteroMap DynMap where
  type Key DynMap = DynKey

  lookup (DynKey k) (DynMap m) = x
    where
      Just x = M.lookup k m >>= fromDyn

  map f (DynMap m) =
      DynMap $ M.map g m
    where
     g dyn = mapDyn f dyn

  mapWithKey f (DynMap m) =
      DynMap $ M.mapWithKey g m
    where
      g k dyn = mapDyn (f (DynKey k)) dyn

  fold f b0 (DynMap m) =
      M.fold g b0 m
    where
      g dyn b = accDyn f dyn b

  foldWithKey f b0 (DynMap m) =
      M.foldWithKey g b0 m
    where
      g k dyn b = accDyn (f (DynKey k)) dyn b

  traverse f (DynMap m) =
      DynMap `fmap` T.traverse g m
    where
      g dyn = appDyn f dyn

  traverseWithKey f (DynMap m) =
      (DynMap . M.fromList) `fmap` T.traverse g (M.toList m)
    where
      g (k, dyn) = (,) k `fmap` (appDyn (f (DynKey k)) dyn)

instance Key2Int DynKey where
  key2Int (DynKey n) = n

empty ∷ DynMap f
empty = DynMap M.empty

insert
  ∷ HasType α
  ⇒ Int
  → f α
  → DynMap f
  → DynMap f
insert k x (DynMap m) = DynMap $ M.insert k (toDyn x) m

toDyn
  ∷ HasType α
  ⇒ f α
  → Dyn f
toDyn = Dyn

fromDyn
  ∷ HasType α
  ⇒ Dyn f
  → Maybe (f α)
fromDyn (Dyn x) =
  -- Proof at runtime that type 'a' is equal to type 'b'
  eqT typeOf typeOf >>= \ w → Just (coerce (cong w) x)

mapDyn
  ∷ (forall α . f α → g α)
  → Dyn f
  → Dyn g
mapDyn f (Dyn x) = Dyn (f x)

accDyn
  ∷ (∀ α . f α → β → β)
  → (Dyn f → β → β)
accDyn f (Dyn x) b = f x b

appDyn
  ∷ Functor t
  ⇒ (∀ α . f α → t (g α))
  → (Dyn f → t (Dyn g))
appDyn f (Dyn x) = Dyn `fmap` (f x)
-}
