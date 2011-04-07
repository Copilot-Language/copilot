{-# LANGUAGE TypeSynonymInstances #-}

-- | Functions for sampling external variables.

module Language.Copilot.Language.Sampling (
        -- * The next functions provide easier access to typed external variables.
        extB, extI8, extI16, extI32, extI64,
        extW8, extW16, extW32, extW64, extF, extD,
        -- * The next functions provide easier access to typed external arrays.
        extArrB, extArrI8, extArrI16, extArrI32, extArrI64,
        extArrW8, extArrW16, extArrW32, extArrW64, extArrF, extArrD,
                                          ) where


import qualified Language.Atom as A
import Data.Int
import Data.Word

import Language.Copilot.Core

class ExtCl a where 
  extB :: a -> Spec Bool
  extI8 :: a -> Spec Int8
  extI16 :: a -> Spec Int16
  extI32 :: a -> Spec Int32
  extI64 :: a -> Spec Int64
  extW8 :: a -> Spec Word8
  extW16 :: a -> Spec Word16
  extW32 :: a -> Spec Word32
  extW64 :: a -> Spec Word64
  extF :: a -> Spec Float
  extD :: a -> Spec Double

  -- for arrays 
  extArrB ::  (Streamable b, A.IntegralE b) => a -> Spec b -> Spec Bool
  extArrI8 :: (Streamable b, A.IntegralE b) => a -> Spec b -> Spec Int8
  extArrI16 :: (Streamable b, A.IntegralE b) => a -> Spec b -> Spec Int16
  extArrI32 :: (Streamable b, A.IntegralE b) => a -> Spec b -> Spec Int32
  extArrI64 :: (Streamable b, A.IntegralE b) => a -> Spec b -> Spec Int64
  extArrW8 :: (Streamable b, A.IntegralE b) => a -> Spec b -> Spec Word8
  extArrW16 :: (Streamable b, A.IntegralE b) => a -> Spec b -> Spec Word16
  extArrW32 :: (Streamable b, A.IntegralE b) => a -> Spec b -> Spec Word32
  extArrW64 :: (Streamable b, A.IntegralE b) => a -> Spec b -> Spec Word64
  extArrF :: (Streamable b, A.IntegralE b) => a -> Spec b -> Spec Float
  extArrD :: (Streamable b, A.IntegralE b) => a -> Spec b -> Spec Double

-- | For global variables.
instance ExtCl String where
  extB var = ExtVar A.Bool (ExtV var)
  extI8 var = ExtVar A.Int8 (ExtV var)
  extI16 var = ExtVar A.Int16 (ExtV var)
  extI32 var = ExtVar A.Int32 (ExtV var)
  extI64 var = ExtVar A.Int64 (ExtV var)
  extW8 var = ExtVar A.Word8 (ExtV var)
  extW16 var = ExtVar A.Word16 (ExtV var)
  extW32 var = ExtVar A.Word32 (ExtV var)
  extW64 var = ExtVar A.Word64 (ExtV var)
  extF var = ExtVar A.Float (ExtV var)
  extD var = ExtVar A.Double (ExtV var)

  -- for arrays 
  extArrB = \v idx -> ExtArr A.Bool (ExtV v, idx)
  extArrI8 = \v idx -> ExtArr A.Int8 (ExtV v, idx)
  extArrI16 = \v idx -> ExtArr A.Int16 (ExtV v, idx)
  extArrI32 = \v idx -> ExtArr A.Int32 (ExtV v, idx)
  extArrI64 = \v idx -> ExtArr A.Int64 (ExtV v, idx)
  extArrW8 = \v idx -> ExtArr A.Word8 (ExtV v, idx)
  extArrW16 = \v idx -> ExtArr A.Word16 (ExtV v, idx)
  extArrW32 = \v idx -> ExtArr A.Word32 (ExtV v, idx)
  extArrW64 = \v idx -> ExtArr A.Word64 (ExtV v, idx)
  extArrF = \v idx -> ExtArr A.Float (ExtV v, idx)
  extArrD = \v idx -> ExtArr A.Double (ExtV v, idx)

-- | For functions.
instance ExtCl Ext where
  extB = ExtVar A.Bool 
  extI8 = ExtVar A.Int8 
  extI16 = ExtVar A.Int16 
  extI32 = ExtVar A.Int32 
  extI64 = ExtVar A.Int64 
  extW8 = ExtVar A.Word8 
  extW16 = ExtVar A.Word16
  extW32 = ExtVar A.Word32
  extW64 = ExtVar A.Word64
  extF = ExtVar A.Float
  extD = ExtVar A.Double
  -- for arrays 
  extArrB = curry (ExtArr A.Bool)
  extArrI8 = curry (ExtArr A.Int8)
  extArrI16 = curry (ExtArr A.Int16)
  extArrI32 = curry (ExtArr A.Int32)
  extArrI64 = curry (ExtArr A.Int64)
  extArrW8 = curry (ExtArr A.Word8)
  extArrW16 = curry (ExtArr A.Word16)
  extArrW32 = curry (ExtArr A.Word32)
  extArrW64 = curry (ExtArr A.Word64)
  extArrF = curry (ExtArr A.Float)
  extArrD = curry (ExtArr A.Double)

