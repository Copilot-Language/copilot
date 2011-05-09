-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Copilot.Array
  ( Array (..)
  ) where

import Data.Typeable (Typeable)

data Array :: * -> * where
  Array :: [a] -> Array a
  deriving (Eq, Show, Typeable)

instance (Num a) => Num (Array a) where
  Array xs + Array ys = Array $ zipWith (+) xs ys
  Array xs * Array ys = Array $ zipWith (*) xs ys
  Array xs - Array ys = Array $ zipWith (-) xs ys
  abs (Array xs)      = Array $ map abs xs
  signum (Array xs)   = Array $ map abs xs
  fromInteger k       = Array $ [fromInteger k]
