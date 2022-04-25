{-# LANGUAGE Safe #-}

-- | Combinators to deal with streams carrying structs.
module Copilot.Language.Operators.Struct
  ( (#)
  ) where

import Copilot.Core.Type
import Copilot.Core.Operators
import Copilot.Language.Stream  (Stream (..))

import GHC.TypeLits             (KnownSymbol)

-- | Create a stream that carries a field of a struct in another stream.
--
-- This function implements a projection of a field of a struct over time. For
-- example, if a struct of type @T@ has two fields, @t1@ of type @Int@ and @t2@
-- of type @Word8@, and @s@ is a stream of type @Stream T@, then @s # t2@ has
-- type @Stream Word8@ and contains the values of the @t2@ field of the structs
-- in @s@ at any point in time.
(#) :: (KnownSymbol s, Typed t, Typed a, Struct a)
      => Stream a -> (a -> Field s t) -> Stream t
(#) s f = Op1 (GetField typeOf typeOf f) s
