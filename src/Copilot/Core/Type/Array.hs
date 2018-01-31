--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

-- | A purposely limited array implementation. Unlike Data.Array, it only
-- supports integer indixed arrays, starting at zero. 

{-# LANGUAGE GADTs, FlexibleInstances, StandaloneDeriving #-}

module Copilot.Core.Type.Array
  ( Array
  , array
  , Index (..)
  , dim
  , fmap
  , toList
  ) where

import Data.List
import Data.Foldable

{- Class used as index / length of an array -}
class Eq a => Index a where
  indices :: a -> [a]

instance Index Int where
  indices i = [0..i-1]

instance Index (Int, Int) where
  indices (i1, i2) = [(i1', i2')  | i1' <- indices i1
                                  , i2' <- indices i2]

instance Index (Int, Int, Int) where
  indices (i1, i2, i3) = [(i1', i2', i3') | i1' <- indices i1
                                          , i2' <- indices i2
                                          , i3' <- indices i3]

instance Index (Int, Int, Int, Int) where
  indices (i1, i2, i3, i4) = [(i1', i2', i3', i4')  | i1' <- indices i1
                                                    , i2' <- indices i2
                                                    , i3' <- indices i3
                                                    , i4' <- indices i4]

data Array i a where
  Array :: (Index i) => i -> [(i, a)] -> Array i a

dim :: Array i a -> i
dim (Array i _) = i

deriving instance (Show i, Show a) => Show (Array i a)

instance Index i => Functor (Array i) where
  fmap f (Array is xs) = Array is (map f' xs) where
    f' (i, x) = (i, f x)

instance Foldable (Array i) where
  foldMap f           = foldr (mappend.f) mempty
  toList (Array _ xs) = map snd xs

array :: (Index i) => i -> [(i, a)] -> Array i a
array i vals | checks_OK = Array i vals
             | otherwise = error "Values and indices do not match in array." where
  checks_OK = length_OK && indices_OK
  length_OK = length is == length vals
  indices_OK = length (nub is') == length is
  is = indices i
  is' = map fst vals  -- Indices given by value tuples
