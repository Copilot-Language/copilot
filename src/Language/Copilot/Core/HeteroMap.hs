-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- | Type-safe heterogenous maps.

{-# LANGUAGE Rank2Types #-}

module Language.Copilot.Core.HeteroMap
  ( Map (..)
  , Key (..)
  ) where

import Language.Copilot.Core.Type (Typed)

class Map φ where

  lookup
    :: Typed α
    => Key
    -> φ f
    -> Maybe (f α)

  map
    :: (forall α . f α -> g α)
    -> φ f
    -> φ g

  mapWithKey
    :: (forall α . Key -> f α -> g α)
    -> φ f
    -> φ g

  fold
    :: (forall α . f α -> β -> β)
    -> β
    -> φ f
    -> β

  foldWithKey
    :: (forall α . Key -> f α -> β -> β)
    -> β
    -> φ f
    -> β

  traverse
    :: (Functor m, Monad m)
    => (forall α . f α -> m (g α))
    -> φ f
    -> m (φ g)

  traverseWithKey
    :: (Functor m, Monad m)
    => (forall α . Key -> f α -> m (g α))
    -> φ f
    -> m (φ g)

newtype Key = Key { keyToInt :: Int }
