-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Safe                      #-}

module System.Mem.StableName.Dynamic
  ( DynStableName(..)
  , hashDynStableName
  , makeDynStableName
  ) where

import System.Mem.StableName (StableName, eqStableName, makeStableName,
                              hashStableName)

data DynStableName = forall a . DynStableName (StableName a)

makeDynStableName :: a -> IO DynStableName
makeDynStableName x =
  do
    stn <- makeStableName x
    return (DynStableName stn)

hashDynStableName :: DynStableName -> Int
hashDynStableName (DynStableName sn) = hashStableName sn

instance Eq DynStableName where
  DynStableName sn1 == DynStableName sn2 = eqStableName sn1 sn2
