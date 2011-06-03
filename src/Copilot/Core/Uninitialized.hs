--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

uninitialized :: Type α -> α
uninitialized t =
  case t of
    Bool   p -> coerce (symm p) False
    Int8   p -> coerce (symm p) 0
    Int16  p -> coerce (symm p) 0
    Int32  p -> coerce (symm p) 0
    Int64  p -> coerce (symm p) 0
    Word8  p -> coerce (symm p) 0
    Word16 p -> coerce (symm p) 0
    Word32 p -> coerce (symm p) 0
    Word64 p -> coerce (symm p) 0

--------------------------------------------------------------------------------