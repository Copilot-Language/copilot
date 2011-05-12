-- |

{-# LANGUAGE TypeFamilies #-}

module Language.Copilot.Core.HeteroMap
  ( HeteroMap (..)
  ) where

import Language.Copilot.Core.Functor2 (Functor2)
import Language.Copilot.Core.Type (Typed)

class Functor2 map => HeteroMap map where

  type Key map :: * -> *

  hlookup :: Typed a => Key map a -> map f -> f a

  key2Int :: Key map a -> Int
