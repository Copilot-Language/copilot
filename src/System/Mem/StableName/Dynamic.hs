--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module System.Mem.StableName.Dynamic
  ( DynStableName(..)
  , hashDynStableName
  , makeDynStableName
  ) where

import System.Mem.StableName (StableName, makeStableName, hashStableName)
import Unsafe.Coerce (unsafeCoerce)

newtype DynStableName = DynStableName (StableName ())

makeDynStableName :: a -> IO DynStableName
makeDynStableName x =
  do
    stn <- makeStableName x
    return (DynStableName (unsafeCoerce stn))

hashDynStableName :: DynStableName -> Int
hashDynStableName (DynStableName sn) = hashStableName sn

instance Eq DynStableName where
  DynStableName sn1 == DynStableName sn2 = sn1 == sn2
