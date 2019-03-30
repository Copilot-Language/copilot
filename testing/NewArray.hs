{-# LANGUAGE DataKinds #-}

module Main where

import Language.Copilot

import Copilot.Compile.C
import qualified Prelude as P

import GHC.TypeLits

--array' :: [a] -> Array n a
array' as = array (zip [0..] as)

arr :: Stream (Array (Len 3) Int8)
arr = [ array' [4,5,6],
        array' [1,2,3] ] ++ arr

--arr' :: Stream (Array (Len 3) Int8)
--arr' = [ array' [10,20,30] ] ++ arr

exarr :: Stream (Array (Len 3) Int8)
exarr = extern "exarr" Nothing

counter :: Stream Int8
counter = [1] ++ (counter + 1)

spec = do
  trigger "farray" true [arg counter, arg arr, arg exarr]

main = do reify spec >>= compile defaultParams
