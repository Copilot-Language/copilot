-- |

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Copilot.Core.Specification
  ( Rank2HeteroMap (..)
  , Specification (..)
  ) where

import Language.Copilot.Core.Node (Node)
import Language.Copilot.Core.Type (Typed)

class Rank2HeteroMap map where

  type Key map :: * -> *

  hlookup :: Typed a => Key map a -> map f -> f a

  hmap :: (forall a . f a -> g a) -> map f -> map g

class Specification spec where

  runSpec
    :: spec a
    -> (
         forall m . Rank2HeteroMap m
           => m (Node (Key m))
           -> Key m a
           -> b
       )
    -> b
