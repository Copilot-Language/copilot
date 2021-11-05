--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Initial values for a given type.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}

module Copilot.Core.Type.Uninitialized
  ( uninitialized
  ) where

import Copilot.Core.Type

--------------------------------------------------------------------------------

-- | Initial value for a given type.
--
-- Does not support structs or arrays.
{-# DEPRECATED uninitialized "This function is deprecated in Copilot 3.6" #-}
uninitialized :: Type a -> a
uninitialized t =
  case t of
    Bool   -> False
    Int8   -> 0
    Int16  -> 0
    Int32  -> 0
    Int64  -> 0
    Word8  -> 0
    Word16 -> 0
    Word32 -> 0
    Word64 -> 0
    Float  -> 0
    Double -> 0
