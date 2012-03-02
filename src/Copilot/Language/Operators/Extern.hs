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
  , externF
  , externD
  , externFun
  , externArray
  , externArrayB
  , externArrayW8
  , externArrayW16
  , externArrayW32
  , externArrayW64
  , externArrayI8
  , externArrayI16
  , externArrayI32
  , externArrayI64
  , externArrayF
  , externArrayD
  , funArg -- ^ Deprecated.
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream
import Data.Word
import Data.Int

type Size = Int

--------------------------------------------------------------------------------

extern :: Typed a => String -> Maybe [a] -> Stream a
extern = Extern

externFun :: Typed a => String -> [Arg] -> Maybe (Stream a) -> Stream a
externFun = ExternFun

externArray :: (Typed a, Typed b, Integral a) 
            => String -> Stream a -> Size -> Maybe [[b]] -> Stream b
externArray = ExternArray

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
externF   :: String -> Maybe [Float] -> Stream Float
externF   = extern
externD   :: String -> Maybe [Double] -> Stream Double
externD   = extern

--------------------------------------------------------------------------------

externArrayB   :: (Typed a, Integral a) 
               => String -> Stream a -> Size 
                         -> Maybe [[Bool]] -> Stream Bool
externArrayB   = externArray 
externArrayW8  :: (Typed a, Integral a) 
               => String -> Stream a -> Size 
                         -> Maybe [[Word8]] -> Stream Word8
externArrayW8  = externArray 
externArrayW16 :: (Typed a, Integral a)
               => String -> Stream a -> Size 
                         -> Maybe [[Word16]] -> Stream Word16
externArrayW16 = externArray
externArrayW32 :: (Typed a, Integral a)
               => String -> Stream a -> Size 
                         -> Maybe [[Word32]] -> Stream Word32
externArrayW32 = externArray
externArrayW64 :: (Typed a, Integral a)
               => String -> Stream a -> Size 
                         -> Maybe [[Word64]] -> Stream Word64
externArrayW64 = externArray
externArrayI8  :: (Typed a, Integral a)
               => String -> Stream a -> Size 
                         -> Maybe [[Int8]] -> Stream Int8
externArrayI8  = externArray
externArrayI16 :: (Typed a, Integral a)
               => String -> Stream a -> Size 
                         -> Maybe [[Int16]] -> Stream Int16
externArrayI16 = externArray
externArrayI32 :: (Typed a, Integral a)
               => String -> Stream a -> Size 
                         -> Maybe [[Int32]] -> Stream Int32
externArrayI32 = externArray
externArrayI64 :: (Typed a, Integral a)
               => String -> Stream a -> Size 
                         -> Maybe [[Int64]] -> Stream Int64
externArrayI64 = externArray
externArrayF   :: (Typed a, Integral a)
               => String -> Stream a -> Size 
                         -> Maybe [[Float]] -> Stream Float
externArrayF   = externArray
externArrayD   :: (Typed a, Integral a)
               => String -> Stream a -> Size 
                         -> Maybe [[Double]] -> Stream Double
externArrayD   = externArray
