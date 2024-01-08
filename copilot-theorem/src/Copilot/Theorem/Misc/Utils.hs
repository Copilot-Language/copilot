{-# LANGUAGE Trustworthy #-}

-- | Utility / auxiliary functions.
module Copilot.Theorem.Misc.Utils
 ( isSublistOf, nub', nubBy', nubEq
 , openTempFile
 ) where

import Data.Function (on)
import Data.List (groupBy, sortBy, group, sort)

import Control.Applicative ((<$>))
import Control.Monad

import qualified Data.Set as Set

import System.IO hiding (openTempFile)
import System.Random
import System.Directory

-- | True if the given list is a subset of the second list, when both are
-- considered as sets.
isSublistOf :: Ord a => [a] -> [a] -> Bool
isSublistOf = Set.isSubsetOf `on` Set.fromList

-- | True if both lists contain the same elements, when both are considered as
-- sets.
nubEq :: Ord a => [a] -> [a] -> Bool
nubEq = (==) `on` Set.fromList

-- | Remove duplicates from a list.
--
-- This is an efficient version of 'Data.List.nub' that works for lists with a
-- stronger constraint on the type (i.e., 'Ord', as opposed of
-- 'Data.List.nub''s 'Eq' constraint).
nub' :: Ord a => [a] -> [a]
nub' = map head . group . sort

-- | Variant of 'nub'' parameterized by the comparison function.
nubBy' :: (a -> a -> Ordering) -> [a] -> [a]
nubBy' f = map head . groupBy (\x y -> f x y == EQ) . sortBy f

-- | Create a temporary file and open it for writing.
openTempFile :: String  -- ^ Directory where the file should be created.
             -> String  -- ^ Base name for the file (prefix).
             -> String  -- ^ File extension.
             -> IO (String, Handle)
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
