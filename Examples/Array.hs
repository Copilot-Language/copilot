--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Example in sampling external arrays.

{-# LANGUAGE RebindableSyntax #-}

module Array ( array ) where

import Language.Copilot hiding (cycle)
import Data.List (cycle)
--import qualified Copilot.Compile.C99 as C
import qualified Copilot.Compile.SBV as S

--------------------------------------------------------------------------------

extArr :: Stream Word32
extArr = externArray "arr1" arrIdx 5 (Just $ repeat [7,8,9,10,11])

arrIdx :: Stream Word32
arrIdx = [0] ++ (arrIdx + 1) `mod` 4

arrIdx2 :: Stream Word8
arrIdx2 = extern "idx2" (Just [0,0..])

extArr2 :: Stream Word16 -> Stream Word32
extArr2 idx = externArray "arr2" idx 4 (Just $ repeat [1,2,3,4])

extArr3 :: Stream Word32
extArr3 = extArr2 (cast $ externW8 "idx3" (Just $ cycle [0,1,2])) 

spec :: Spec
spec = trigger "trigger" true [ arg extArr
                              , arg (extArr2 (cast arrIdx2))
                              , arg extArr3
-- Throws an exception since the index is too big for the array!
--                              , arg (extArr2 5)
                              ]

array :: IO ()
array = do
--  reify spec >>= C.compile C.defaultParams 
  interpret 10 spec
  reify spec >>= S.compile S.defaultParams 

--------------------------------------------------------------------------------
