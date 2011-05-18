-- |

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module Language.Copilot.Core.HeteroMap
  ( HeteroMap (..)
  ) where

import Control.Applicative (Applicative)
import Data.Monoid (Monoid)
import Language.Copilot.Core.Type (HasType)

class HeteroMap map where

  type Key map :: * -> *

  key2int
    :: Key map a
    -> Int

  lookup
    :: HasType a
    => Key map a
    -> map f
    -> f a

  mapWithKey
    :: (forall a . Key map a -> f a -> g a)
    -> map f
    -> map g

  foldMapWithKey
    :: Monoid m
    => (forall a . Key map a -> f a -> m)
    -> map f
    -> m

  traverseWithKey
    :: Applicative t
    => (forall a . Key map a -> f a -> t (g a))
    -> map f
    -> t (map g)
