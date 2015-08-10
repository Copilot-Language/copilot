--------------------------------------------------------------------------------

module Copilot.Theorem.Misc.Utils
 ( isSublistOf, nub', nubBy', nubEq
 , openTempFile
 ) where

--------------------------------------------------------------------------------

import Data.Function (on)
import Data.List (groupBy, sortBy, group, sort)

import Control.Applicative ((<$>))
import Control.Monad

import qualified Data.Set as Set

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
