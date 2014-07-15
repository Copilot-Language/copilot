--------------------------------------------------------------------------------

module Copilot.Kind.Misc.Utils 
 ( module Data.Maybe
 , module Control.Monad
 , printf
 , assert
 , Monoid, (<>), mempty, mconcat
 , (<$>), (<*>), liftA, liftA2
 , nub, partition, groupBy, sortBy, intercalate, find, (\\)
 , on
 , (!)
 , Map, Bimap
 , Set, isSubsetOf, member
 -- Some personnal functions
 , fst3, snd3, thrd3
 , isSublistOf, nub', nubBy', nubEq
 ) where

--------------------------------------------------------------------------------

import Control.Exception.Base (assert)

import Data.Maybe
import Data.Monoid (Monoid, (<>), mempty, mconcat)

import Data.Function (on)
import Data.List (nub, partition, groupBy, sortBy, 
                  intercalate, find, (\\), group, sort)

import Control.Applicative ((<$>), (<*>), liftA, liftA2)
import Control.Monad

import Data.Map ((!), Map)
import Data.Bimap (Bimap)
import Data.Set (Set, isSubsetOf, member)

import qualified Data.Set as Set

import Text.Printf (printf)

--------------------------------------------------------------------------------

fst3  (a, _, _) = a
snd3  (_, b, _) = b
thrd3 (_, _, c) = c

isSublistOf :: Ord a => [a] -> [a] -> Bool
isSublistOf = Set.isSubsetOf `on` Set.fromList

nubEq :: Ord a => [a] -> [a] -> Bool
nubEq = (==) `on` Set.fromList

-- An efficient version of 'nub'
nub' :: Ord a => [a] -> [a]
nub' = map head . group . sort

nubBy' :: (a -> a -> Ordering) -> [a] -> [a]
nubBy' f = map head . groupBy (\x y -> f x y == EQ) . sortBy f

--------------------------------------------------------------------------------
