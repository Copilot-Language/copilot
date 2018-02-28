--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

-- | A purposely limited array implementation. Unlike Data.Array, it only
-- supports integer indixed arrays, starting at zero.

{-# LANGUAGE  GADTs
            , FlexibleInstances
            , PolyKinds
            , ScopedTypeVariables
            , MultiParamTypeClasses
            , FunctionalDependencies
            , InstanceSigs
#-}

module Copilot.Core.Type.Array
  ( Array
  , array
  , toList
  , fmap
  , foldr
  , dim
  , Len
  , Index (..)
  , (.!)
  ) where

import GHC.TypeLits
import Data.Foldable (toList)

{- Take proxy and turn it into an Int -}
fromNat :: KnownNat n => proxy n -> Int
fromNat p = fromInteger $ natVal p

{- Array range up to the proxy -}
idxRange :: KnownNat n => proxy n -> [Int]
idxRange p = [0.. (fromNat p) - 1]


{- Analogous to Proxy, but with a nicer name to hide details -}
data Len a = Len

class Index i n | i -> n where
  index     :: i
  fromIndex :: i -> [n]
  size      :: i -> Int
  size i = length $ fromIndex i

instance KnownNat n => Index (Len n) Int where
  index     = Len
  fromIndex = idxRange

instance (KnownNat m, KnownNat n) => Index (Len m, Len n) (Int, Int) where
  index = (Len, Len)
  fromIndex (m, n) = [ (m', n') | m' <- idxRange m
                                , n' <- idxRange n ]


{- Actual Array data type -}
data Array i a where
  Array :: Index i n => i -> [(n, a)] -> Array i a

instance Functor (Array i) where
  fmap f (Array i as) = Array i (map (applysnd f) as) where
    applysnd :: (b -> c) -> (a, b) -> (a, c)
    applysnd f (a, b) = (a, f b)

instance Foldable (Array i) where
  foldr f b (Array _ as) = foldr (f.snd) b as
  toList (Array _ as) = map snd as


{- Smart constructor for an array, checks validaty of data as well -}
array :: forall n i a. Index i n => [(n, a)] -> Array i a
array xs | length xs == l = Array idx xs
         | otherwise = error $ "Length of data does not match type of array." where
  idx = index
  l = size idx

{- Takes the index element from an array -}
dim :: Array i a -> i
dim (Array i _) = i

(.!) :: Array i a -> Int -> a
(Array _ xs) .! n = snd $ xs !! n
