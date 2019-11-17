--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | External variables, arrays, and functions.

{-# LANGUAGE Safe #-}

module Copilot.Language.Operators.Extern
  ( extern
  , externB
  , externW8
  , externW16
  , externW32
  , externW64
  , externI8
  , externI16
  , externI32
  , externI64
  , externD
  , funArg -- * Deprecated.
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream
import Data.Word
import Data.Int

type Size = Int

--------------------------------------------------------------------------------

extern :: Typed a => String -> Maybe [a] -> Stream a
extern = Extern

-- | Deprecated.
funArg :: Typed a => Stream a -> Arg
funArg = Arg

--------------------------------------------------------------------------------

externB   :: String -> Maybe [Bool] -> Stream Bool
externB   = extern
externW8  :: String -> Maybe [Word8] -> Stream Word8
externW8  = extern
externW16 :: String -> Maybe [Word16] -> Stream Word16
externW16 = extern
externW32 :: String -> Maybe [Word32] -> Stream Word32
externW32 = extern
externW64 :: String -> Maybe [Word64] -> Stream Word64
externW64 = extern
externI8  :: String -> Maybe [Int8] -> Stream Int8
externI8  = extern
externI16 :: String -> Maybe [Int16] -> Stream Int16
externI16 = extern
externI32 :: String -> Maybe [Int32] -> Stream Int32
externI32 = extern
externI64 :: String -> Maybe [Int64] -> Stream Int64
externI64 = extern
externD   :: String -> Maybe [Double] -> Stream Double
externD   = extern
