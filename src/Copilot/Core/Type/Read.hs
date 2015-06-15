--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Copilot.Core.Type.Read
  ( ReadWit (..)
  , readWit
  , readWithType
  ) where

import Copilot.Core.Type
import Copilot.Core.Error (badUsage)

--------------------------------------------------------------------------------

data ReadWit a = Read a => ReadWit

--------------------------------------------------------------------------------

readWit :: Type a -> ReadWit a
readWit t =
  case t of
    Bool   -> ReadWit
    Int8   -> ReadWit
    Int16  -> ReadWit
    Int32  -> ReadWit
    Int64  -> ReadWit
    Word8  -> ReadWit
    Word16 -> ReadWit
    Word32 -> ReadWit
    Word64 -> ReadWit
    Float  -> ReadWit
    Double -> ReadWit
    Struct -> ReadWit

--------------------------------------------------------------------------------

readWithType :: Type a -> String -> a
readWithType t str =
  case t of
    Bool -> case str of
              "0" -> False
              "1" -> True
              x   -> badUsage $ "in readWithType in copilot-core: expecting a \"0\" or \"1\" when reading a Boolean value " ++ show x ++ "."
    _    -> rd
  where
  rd = case readWit t of
         ReadWit -> read str

--------------------------------------------------------------------------------
