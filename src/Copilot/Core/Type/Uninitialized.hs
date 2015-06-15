--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Initial values for give types.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}

module Copilot.Core.Type.Uninitialized
  ( uninitialized
  ) where

import Copilot.Core.Type

--------------------------------------------------------------------------------

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
    Struct -> Undefined
