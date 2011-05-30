-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | 

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Uninitializable
  ( Uninitializable (..)
  ) where

import Data.Int
import Data.Word

class Uninitializable α where
  uninitialized :: α

instance Uninitializable Bool   where uninitialized = False
instance Uninitializable Int8   where uninitialized = 0
instance Uninitializable Int16  where uninitialized = 0
instance Uninitializable Int32  where uninitialized = 0
instance Uninitializable Int64  where uninitialized = 0
instance Uninitializable Word8  where uninitialized = 0
instance Uninitializable Word16 where uninitialized = 0
instance Uninitializable Word32 where uninitialized = 0
instance Uninitializable Word64 where uninitialized = 0

instance
  ( Uninitializable α
  , Uninitializable β
  ) => Uninitializable (α, β) where
    uninitialized = (uninitialized, uninitialized)

instance
  ( Uninitializable α
  , Uninitializable β
  , Uninitializable γ
  ) => Uninitializable (α, β, γ) where
    uninitialized = (uninitialized, uninitialized, uninitialized)

instance
  ( Uninitializable α
  , Uninitializable β
  , Uninitializable γ
  , Uninitializable δ
  ) => Uninitializable (α, β, γ, δ) where
    uninitialized = (uninitialized, uninitialized, uninitialized, uninitialized)
