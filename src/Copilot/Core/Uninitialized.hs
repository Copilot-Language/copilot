-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | 

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core.Uninitialized
  ( uninitialized
  ) where

import Copilot.Core.Type
import Copilot.Core.Type.Equality
import Data.Int
import Data.Word

class U α where
  u :: α

instance U Bool   where u = False
instance U Int8   where u = 0
instance U Int16  where u = 0
instance U Int32  where u = 0
instance U Int64  where u = 0
instance U Word8  where u = 0
instance U Word16 where u = 0
instance U Word32 where u = 0
instance U Word64 where u = 0
instance U Float  where u = 0
instance U Double where u = 0

data UInst α = U α => UInst

mkUInst :: U β => Equal α β -> UInst α
mkUInst = (`coerce` UInst) . cong . symm

uInst :: Type α -> UInst α
uInst t =
  case t of
    Bool   p -> mkUInst p
    Int8   p -> mkUInst p
    Int16  p -> mkUInst p
    Int32  p -> mkUInst p
    Int64  p -> mkUInst p
    Word8  p -> mkUInst p
    Word16 p -> mkUInst p
    Word32 p -> mkUInst p
    Word64 p -> mkUInst p
    Float  p -> mkUInst p
    Double p -> mkUInst p

uninitialized :: Type α -> α
uninitialized t = case uInst t of UInst -> u
