-- |

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Copilot.Core.Array
  ( Array
  , indexArray
  , newArray
  ) where

import Control.DeepSeq (NFData)

newtype Array a = Array [a]
  deriving
    ( Eq
    , NFData
    , Show
    )

instance (Num a) => Num (Array a) where
  Array xs + Array ys = Array $ zipWith (+) xs ys
  Array xs * Array ys = Array $ zipWith (*) xs ys
  Array xs - Array ys = Array $ zipWith (-) xs ys
  abs (Array xs)      = Array $ map abs xs
  signum (Array xs)   = Array $ map abs xs
  fromInteger k       = Array $ [fromInteger k]

indexArray :: Integral i => i -> Array a -> a
indexArray i (Array xs) = xs !! (fromIntegral i)

newArray :: [a] -> Array a
newArray xs = Array xs
