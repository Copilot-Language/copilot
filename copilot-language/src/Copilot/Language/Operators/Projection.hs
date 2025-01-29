{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
-- | Interface to access portions of a larger data structure.
--
-- Default operations to access elements from structs and arrays (e.g., a field
-- of a struct, an element of an array) allow obtaining the values of those
-- elements, but not modifying.
--
-- This module defines a common interface to manipulate portions of a larger
-- data structure. We define the interface in a generic way, using a type
-- class with two operations: one to set the value of the sub-element, and
-- one to update the value of such element applying a stream function.
module Copilot.Language.Operators.Projection
    ( Projectable (..) )
  where

import Copilot.Language.Stream (Stream)

infixl 8 =:
infixl 8 =$

-- | Common interface to manipulate portions of a larger data structure.
--
-- A projectable d s t means that it is possible to manipulate a sub-element s
-- of type t carried in a stream of type d.
class Projectable d s t | d s -> t where

  -- | Unapplied projection or element access on a type.
  data Projection d s t

  -- | Modify the value of a sub-element of a type in a stream of elements
  -- of that type.
  (=:) :: Projection d s t -> Stream t -> Stream d

  -- | Update the value of a sub-element of a type in a stream of elements of
  -- that type, by applying a function on streams.
  (=$) :: Projection d s t -> (Stream t -> Stream t) -> Stream d
