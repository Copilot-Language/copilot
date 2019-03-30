--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Example in sampling external arrays.

{-# LANGUAGE RebindableSyntax, DataKinds #-}

module Array ( array ) where

import Language.Copilot hiding (cycle)
import Copilot.Compile.C

import Examples.Util

{- Small helper function for constructing 1-dimensional arrays -}
array' as = array (zip [0..] as)

arr :: Stream (Array (Len 3) Int8)
arr = [ array' [4,5,6],
        array' [1,2,3] ] ++ arr

exarr :: Stream (Array (Len 3) Int8)
exarr = extern "exarr" Nothing

arr2D :: Stream (Array (Len 2, Len 3) Int8)
arr2D = [ array [ ((0,0), 10), ((0,1), 20), ((0,2),30)
                , ((1,0), 40), ((1,1), 50), ((1,2),40)
                ] ] ++ arr2D

spec = do
  trigger "farray" true [arg arr, arg arr]
  trigger "farray2D" true [arg arr2D]

main = interpret 30 spec
