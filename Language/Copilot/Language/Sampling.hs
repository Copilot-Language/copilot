{-# LANGUAGE TypeSynonymInstances #-}

-- | Functions for sampling external variables.

module Language.Copilot.Language.Sampling (
        -- * The next functions provide easier access to typed external variables.
        extB, extI8, extI16, extI32, extI64,
        extW8, extW16, extW32, extW64, extF, extD,
        -- * The next functions provide easier access to typed external arrays.
        extArrB, extArrI8, extArrI16, extArrI32, extArrI64,
        extArrW8, extArrW16, extArrW32, extArrW64, extArrF, extArrD,
        fun
                                          ) where


import qualified Language.Atom as A
import Data.Int
import Data.Word

import Language.Copilot.Language.FunctionCalls
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
  extB var = PVar A.Bool (ExtV var)
  extI8 var = PVar A.Int8 (ExtV var)
  extI16 var = PVar A.Int16 (ExtV var)
  extI32 var = PVar A.Int32 (ExtV var)
  extI64 var = PVar A.Int64 (ExtV var)
  extW8 var = PVar A.Word8 (ExtV var)
  extW16 var = PVar A.Word16 (ExtV var)
  extW32 var = PVar A.Word32 (ExtV var)
  extW64 var = PVar A.Word64 (ExtV var)
  extF var = PVar A.Float (ExtV var)
  extD var = PVar A.Double (ExtV var)

  -- for arrays 
  extArrB = \v idx -> PArr A.Bool (ExtV v, idx)
  extArrI8 = \v idx -> PArr A.Int8 (ExtV v, idx)
  extArrI16 = \v idx -> PArr A.Int16 (ExtV v, idx)
  extArrI32 = \v idx -> PArr A.Int32 (ExtV v, idx)
  extArrI64 = \v idx -> PArr A.Int64 (ExtV v, idx)
  extArrW8 = \v idx -> PArr A.Word8 (ExtV v, idx)
  extArrW16 = \v idx -> PArr A.Word16 (ExtV v, idx)
  extArrW32 = \v idx -> PArr A.Word32 (ExtV v, idx)
  extArrW64 = \v idx -> PArr A.Word64 (ExtV v, idx)
  extArrF = \v idx -> PArr A.Float (ExtV v, idx)
  extArrD = \v idx -> PArr A.Double (ExtV v, idx)

-- | For functions.
instance ExtCl Ext where
  extB = PVar A.Bool 
  extI8 = PVar A.Int8 
  extI16 = PVar A.Int16 
  extI32 = PVar A.Int32 
  extI64 = PVar A.Int64 
  extW8 = PVar A.Word8 
  extW16 = PVar A.Word16
  extW32 = PVar A.Word32
  extW64 = PVar A.Word64
  extF = PVar A.Float
  extD = PVar A.Double
  -- for arrays 
  extArrB = \fn idx -> PArr A.Bool (fn, idx)
  extArrI8 = \fn idx -> PArr A.Int8 (fn, idx)
  extArrI16 = \fn idx -> PArr A.Int16 (fn, idx)
  extArrI32 = \fn idx -> PArr A.Int32 (fn, idx)
  extArrI64 = \fn idx -> PArr A.Int64 (fn, idx)
  extArrW8 = \fn idx -> PArr A.Word8 (fn, idx)
  extArrW16 = \fn idx -> PArr A.Word16 (fn, idx)
  extArrW32 = \fn idx -> PArr A.Word32 (fn, idx)
  extArrW64 = \fn idx -> PArr A.Word64 (fn, idx)
  extArrF = \fn idx -> PArr A.Float (fn, idx)
  extArrD = \fn idx -> PArr A.Double (fn, idx)

