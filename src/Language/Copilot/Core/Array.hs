-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Copilot.Core.Array
  ( Array (..)
  ) where

import Control.DeepSeq (NFData)
--import Data.Typeable (Typeable)

newtype Array a = Array [a]
  deriving
    ( Eq
    , NFData
    , Show
--    , Typeable
    )

instance (Num a) => Num (Array a) where
  Array xs + Array ys = Array $ zipWith (+) xs ys
  Array xs * Array ys = Array $ zipWith (*) xs ys
  Array xs - Array ys = Array $ zipWith (-) xs ys
  abs (Array xs)      = Array $ map abs xs
  signum (Array xs)   = Array $ map abs xs
  fromInteger k       = Array $ [fromInteger k]
