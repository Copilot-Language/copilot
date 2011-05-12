-- |

{-# LANGUAGE Rank2Types #-}

module Language.Copilot.Core.Specification
  ( Specification (..)
  ) where

import Language.Copilot.Core.HeteroMap (HeteroMap (..))
import Language.Copilot.Core.Node (Node)

class Specification spec where

  runSpec
    :: spec a
    -> (
         forall m . HeteroMap m
           => m (Node (Key m))
           -> Key m a
           -> b
       )
    -> b
