{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE TypeFamilies          #-}
-- The following warning is disabled due to a necessary instance of Projectable
-- defined in this module.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Combinators to deal with streams carrying arrays.
module Copilot.Language.Operators.Array
  ( (!)
  , (!!)
  , (=:)
  , (=$)
  ) where

import Copilot.Core                          (Array, Op2 (Index),
                                              Op3 (UpdateArray), Typed, typeOf)
import Copilot.Language.Operators.Projection (Projectable(..))
import Copilot.Language.Stream               (Stream (..))

import Data.Word    (Word32)
import GHC.TypeLits (KnownNat)
import Prelude      hiding ((!!))

-- | Create a stream that carries an element of an array in another stream.
--
-- This function implements a projection of the element of an array at a given
-- position, over time. For example, if @s@ is a stream of type @Stream (Array
-- '5 Word8)@, then @s ! 3@ has type @Stream Word8@ and contains the 3rd
-- element (starting from zero) of the arrays in @s@ at any point in time.
(!) :: (KnownNat n, Typed t)
    => Stream (Array n t) -> Stream Word32 -> Stream t
arr ! n = Op2 (Index typeOf) arr n

-- | Pair a stream with an element accessor, without applying it to obtain the
-- value of the element.
--
-- This function is needed to refer to an element accessor when the goal is to
-- update the element value, not just to read it.
(!!) :: Stream (Array n t)
     -> Stream Word32
     -> Projection (Array n t) (Stream Word32) t
(!!) = ProjectionA

-- | Update a stream of arrays.

-- This is an orphan instance; we suppress the warning that GHC would
-- normally produce with a GHC option at the top.
instance (KnownNat n, Typed t) => Projectable (Array n t) (Stream Word32) t where

  -- | A projection of an element of a stream of arrays.
  data Projection (Array n t) (Stream Word32) t =
       ProjectionA (Stream (Array n t)) (Stream Word32)

  -- | Create a stream where an element of an array has been updated with
  -- values from another stream.
  --
  -- For example, if an array has two elements of type @Int32@, and @s@ is a
  -- stream of such array type (@Stream (Array 2 Int32)@), and $v0$ is a stream
  -- of type @Int32@, then @s !! 0 =: v0@ has type @Stream (Array 2 Int32)@ and
  -- contains arrays where the value of the first element of each array is that
  -- of @v0@ at each point in time, and the value of the second element in the
  -- array is the same it had in @s@.
  (=:) (ProjectionA s ix) v = Op3 (UpdateArray typeOf) s ix v

  -- | Create a stream where an element of an array has been updated by
  -- applying a stream function to it.
  --
  -- For example, if an array has two elements of type @Int32@, and @s@ is a
  -- stream of such array type (@Stream (Array 2 Int32)@), and $f$ is function
  -- of type @Stream Int32 -> Stream Int32@, then @s !! 0 =$ f@ has type
  -- @Stream (Array 2 Int32)@ and contains arrays where the value of the first
  -- element of each array is that of @f (s ! 0)@ at each point in time, and
  -- the value of the second element in the array is the same it had in @s@.
  (=$) (ProjectionA s ix) op = Op3 (UpdateArray typeOf) s ix (op (s ! ix))
