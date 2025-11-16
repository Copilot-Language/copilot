{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,19,0)
{-# LANGUAGE Trustworthy #-}
#else
{-# LANGUAGE Safe #-}
#endif

-- | Utility / auxiliary functions.
module Copilot.Theorem.Misc.Utils
 ( isSublistOf, nub', nubBy', nubEq
 , openTempFile
 ) where

import Data.Function (on)
import qualified Data.List.NonEmpty as NE
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
nub' = maybe [] (fmap NE.head . NE.group . NE.sort) . NE.nonEmpty

-- | Variant of 'nub'' parameterized by the comparison function.
nubBy' :: (a -> a -> Ordering) -> [a] -> [a]
nubBy' f = maybe [] (fmap NE.head . NE.groupBy (\x y -> f x y == EQ) . NE.sortBy f) . NE.nonEmpty

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
