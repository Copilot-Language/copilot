--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE Safe                      #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | An implementation of dynamic types using "Copilot.Core.Type.Equality".
-- The theory behind this technique is described the following paper:
--
-- * Baars, Arthur I. and Swierstra, S. Doaitse,
-- \"/Typing dynamic typing/\",
-- ACM SIGPLAN Notices vol. 37, p. 157-166, 2002

module Copilot.Core.Type.Dynamic
  {-# DEPRECATED "This module is deprecated in Copilot 3.8." #-}
  ( Dynamic  (..)
  , DynamicF (..)
  , toDyn
  , fromDyn
  ) where

import Copilot.Core.Type.Equality

--------------------------------------------------------------------------------

-- | Representation of a value accompanied by its type.
data Dynamic :: (* -> *) -> * where
  Dynamic :: a -> t a -> Dynamic t

-- | Representation of a function accompanied by its type.
data DynamicF :: (* -> *) -> (* -> *) -> * where
  DynamicF :: f a -> t a -> DynamicF f t

-- | Enclose a value and its type in a container.
toDyn :: t a -> a -> Dynamic t
toDyn t x = Dynamic x t

-- | Extract a value from a dynamic. Return 'Nothing' if the value is not of
-- the given type.
fromDyn :: EqualType t => t a -> Dynamic t -> Maybe a
fromDyn t1 (Dynamic x t2) =
  case t1 =~= t2 of
    Just Refl -> return x
    Nothing   -> Nothing
