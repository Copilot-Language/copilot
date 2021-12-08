--------------------------------------------------------------------------------
-- Copyright © 2021 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | An example of a specification using the 'signum' function.

{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Data.List as List

import Copilot.Compile.C99
import Language.Copilot

nums :: [Int32]
nums = [-2, -1, 0, 1, 2]

spec :: Spec
spec = do
  let stream :: Stream Int32
      stream = extern "stream" (Just nums)

  trigger "func" true [arg (signum stream)]

main :: IO ()
main = do
  spec' <- reify spec
  compile "signum" spec'
  interpret (List.genericLength nums) spec
