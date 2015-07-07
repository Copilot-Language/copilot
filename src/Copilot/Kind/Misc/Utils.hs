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
 , isSublistOf, nub', nubBy', nubEq

 , openTempFile
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

import System.IO hiding (openTempFile)
import System.Random
import System.Directory

--------------------------------------------------------------------------------

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

openTempFile :: String -> String -> String -> IO (String, Handle)
openTempFile loc baseName extension = do

  path   <- freshPath
  handle <- openFile path WriteMode
  return (path, handle)

  where

    freshPath :: IO FilePath
    freshPath = do
      path   <- pathFromSuff <$> randSuff
      exists <- doesFileExist path
      if exists then freshPath else return path

    randSuff :: IO String
    randSuff = replicateM 4 $ randomRIO ('0', '9')

    pathFromSuff :: String -> FilePath
    pathFromSuff suf = loc ++ "/" ++ baseName ++ suf ++ "." ++ extension

--------------------------------------------------------------------------------
