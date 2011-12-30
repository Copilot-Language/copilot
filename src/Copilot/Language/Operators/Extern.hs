--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

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
  , FunArg
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
  , funArg
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream
import Data.Word
import Data.Int

type Size = Int

--------------------------------------------------------------------------------

extern :: Typed a => String -> Stream a
extern = Extern

externFun :: Typed a => String -> [FunArg] -> Maybe (Stream a) -> Stream a
externFun = ExternFun

externArray :: (Typed a, Typed b, Integral a) 
            => String -> Stream a -> Size -> Stream b
externArray = ExternArray

funArg :: Typed a => Stream a -> FunArg
funArg = FunArg

--------------------------------------------------------------------------------

externB   :: String -> Stream Bool
externB   = extern 
externW8  :: String -> Stream Word8
externW8  = extern 
externW16 :: String -> Stream Word16
externW16 = extern
externW32 :: String -> Stream Word32
externW32 = extern
externW64 :: String -> Stream Word64
externW64 = extern
externI8  :: String -> Stream Int8
externI8  = extern
externI16 :: String -> Stream Int16
externI16 = extern
externI32 :: String -> Stream Int32
externI32 = extern
externI64 :: String -> Stream Int64
externI64 = extern
externF   :: String -> Stream Float
externF   = extern
externD   :: String -> Stream Double
externD   = extern

--------------------------------------------------------------------------------

externArrayB   :: (Typed a, Integral a) 
               => String -> Stream a -> Size -> Stream Bool
externArrayB   = externArray 
externArrayW8  :: (Typed a, Integral a) 
               => String -> Stream a -> Size -> Stream Word8
externArrayW8  = externArray 
externArrayW16 :: (Typed a, Integral a)
               => String -> Stream a -> Size -> Stream Word16
externArrayW16 = externArray
externArrayW32 :: (Typed a, Integral a)
               => String -> Stream a -> Size -> Stream Word32
externArrayW32 = externArray
externArrayW64 :: (Typed a, Integral a)
               => String -> Stream a -> Size -> Stream Word64
externArrayW64 = externArray
externArrayI8  :: (Typed a, Integral a)
               => String -> Stream a -> Size -> Stream Int8
externArrayI8  = externArray
externArrayI16 :: (Typed a, Integral a)
               => String -> Stream a -> Size -> Stream Int16
externArrayI16 = externArray
externArrayI32 :: (Typed a, Integral a)
               => String -> Stream a -> Size -> Stream Int32
externArrayI32 = externArray
externArrayI64 :: (Typed a, Integral a)
               => String -> Stream a -> Size -> Stream Int64
externArrayI64 = externArray
externArrayF   :: (Typed a, Integral a)
               => String -> Stream a -> Size -> Stream Float
externArrayF   = externArray
externArrayD   :: (Typed a, Integral a)
               => String -> Stream a -> Size ->  Stream Double
externArrayD   = externArray
