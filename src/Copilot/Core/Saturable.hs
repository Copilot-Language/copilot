-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | 

module Copilot.Core.Saturable
  ( Saturable (..)
  ) where

import Data.Int
import Data.Word

class Saturable α where
  saturate :: α -> α

instance Saturable Bool   where saturate = id
instance Saturable Int8   where saturate = id
instance Saturable Int16  where saturate = id
instance Saturable Int32  where saturate = id
instance Saturable Int64  where saturate = id
instance Saturable Word8  where saturate = id
instance Saturable Word16 where saturate = id
instance Saturable Word32 where saturate = id
instance Saturable Word64 where saturate = id

instance (Saturable α, Saturable β) =>
  Saturable (α, β) where
    saturate (x, y) =
      saturate x `seq`
      saturate y `seq`
      (x, y)

instance (Saturable α, Saturable β, Saturable γ) =>
  Saturable (α, β, γ) where
    saturate (x, y, z) =
      saturate x `seq`
      saturate y `seq`
      saturate z `seq`
      (x, y, z)

instance (Saturable α, Saturable β, Saturable γ, Saturable δ) =>
  Saturable (α, β, γ, δ) where
    saturate (x, y, z, w) =
      saturate x `seq`
      saturate y `seq`
      saturate z `seq`
      saturate w `seq`
      (x, y, z, w)
