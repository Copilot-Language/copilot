{-# LANGUAGE Safe #-}

-- | Combinators to deal with streams carrying structs.
--
-- We support two kinds of operations on structs: reading the fields of structs
-- and modifying the fields of structs.
--
-- To obtain the values of field @x@ of a struct @s@, you can just write:
--
-- @
-- expr = s # x
-- @
--
-- If you want to update it, use instead a double hash to refer to the field.
-- You can either update the field:
--
-- @
-- expr = s ## x =: 75
-- @
--
-- To update it by applying a function to it, for example, the function that
-- updates a stream by one unit, just do:
--
-- @
-- expr = s ## x =$ (+1)
-- @
module Copilot.Language.Operators.Struct
  ( (#)
  , Projection
  , (##)
  , (=:)
  , (=$)
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
(#) :: (KnownSymbol f, Typed t, Typed s, Struct s)
      => Stream s -> (s -> Field f t) -> Stream t
(#) s f = Op1 (GetField typeOf typeOf f) s

-- | Type represented an unapplied struct field accessor and a stream carrying
-- structs.
data Projection s f t = Projection (Stream s) (s -> Field f t)

-- | Pair a stream with a field accessor, without applying it to obtain the
-- value of the field.
--
-- This function is needed to refer to a field accessor when the goal is to
-- update the field value, not just to read it.
(##) :: (KnownSymbol f, Typed t, Typed s, Struct s)
     => Stream s -> (s -> Field f t) -> Projection s f t
(##) = Projection

-- | Create a stream where the field of a struct has been updated with values
-- from another stream.
--
-- For example, if a struct of type @T@ has two fields, @t1@ of type @Int32@
-- and @t2@ of type @Word8@, and @s@ is a stream of type @Stream T@, and $sT1$
-- is a stream of type @Int32@ then @s ## t2 =: sT1@ has type @Stream T@ and
-- contains structs where the value of @t1@ is that of @sT1@ and the value of
-- @t2@ is the value that the same field had in @s@, at any point in time.
infixl 8 =:
(=:) :: (KnownSymbol f, Typed t, Typed s, Struct s)
     => Projection s f t -> Stream t -> Stream s
(=:) (Projection s f) v = Op2 (UpdateField typeOf typeOf f) s v

-- | Create a stream where the field of a struct has been updated by applying a
-- function to it.
--
-- For example, if a struct of type @T@ has two fields, @t1@ of type @Int32@
-- and @t2@ of type @Word8@, and @s@ is a stream of type @Stream T@, and $f$ is
-- a function from @Stream Int32 -> Stream Int32@ then @s ## t2 =$ f@ has type
-- @Stream T@ and contains structs where the value of @t1@ is that of @f@
-- applied to the original value of @t1@ in @s@, and the value of @t2@ is the
-- value that the same field had in @s@, at any point in time.
infixl 8 =$
(=$) :: (KnownSymbol f, Typed t, Typed s, Struct s)
     => Projection s f t -> (Stream t -> Stream t) -> Stream s
(=$) (Projection s f) op = s ## f =: (op (s # f))
