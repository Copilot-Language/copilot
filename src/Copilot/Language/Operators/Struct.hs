{-# LANGUAGE Safe #-}

module Copilot.Language.Operators.Struct
  ( (#)
  ) where

import Copilot.Core.Type
import Copilot.Core.Operators
import Copilot.Language.Stream  (Stream (..))

import GHC.TypeLits             (KnownSymbol)

--------------------------------------------------------------------------------

(#) :: (KnownSymbol s, Typed t, Typed a, Struct a)
      => Stream a -> (a -> Field s t) -> Stream t
(#) s f = Op1 (GetField typeOf typeOf (accessorname f)) s

--------------------------------------------------------------------------------
