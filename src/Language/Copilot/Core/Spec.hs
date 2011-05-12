-- |

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Copilot.Core.Spec
  ( Rank2HeteroMap (..)
  , RunSpec
  , Spec (..)
  ) where

import Language.Copilot.Core.Node (Node)
import Language.Copilot.Core.Type (Typed)

class Rank2HeteroMap m where
  type Key m :: * -> *
  hlookup :: Typed a => Key m a -> m f -> f a
  hmap :: (forall a . f a -> g a) -> m f -> m g

type RunSpec a =
  forall b .
    (
      forall m . Rank2HeteroMap m
        => m (Node (Key m))
        -> Key m a
        -> b
    )
      -> b

data Spec a = Spec (RunSpec a)
