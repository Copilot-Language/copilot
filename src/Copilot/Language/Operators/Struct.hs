{-# LANGUAGE Safe #-}

module Copilot.Language.Operators.Struct
  ( field
  ) where

import Copilot.Core.Type
import Copilot.Core.Operators
import Copilot.Language.Stream  (Stream (..))

import GHC.TypeLits             (KnownSymbol)

--------------------------------------------------------------------------------

field :: (KnownSymbol s, Typed t, Typed a, Struct a)
      => Stream a -> (a -> Field s t) -> Stream t
field s f = Op1 (GetField typeOf typeOf (accessorname f)) s

--------------------------------------------------------------------------------
