--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Constants.

{-# LANGUAGE Trustworthy #-}

module Copilot.Language.Operators.Constant
  ( constant
  , constB
  , constW8
  , constW16
  , constW32
  , constW64
  , constI8
  , constI16
  , constI32
  , constI64
  , constF
  , constD
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream

import Data.Word
import Data.Int

--------------------------------------------------------------------------------

constant :: Typed a => a -> Stream a
constant = Const

--------------------------------------------------------------------------------

constB   :: Bool -> Stream Bool
constB   = constant
constW8  :: Word8 -> Stream Word8
constW8  = constant 
constW16 :: Word16 -> Stream Word16
constW16 = constant
constW32 :: Word32 -> Stream Word32
constW32 = constant
constW64 :: Word64 -> Stream Word64
constW64 = constant
constI8  :: Int8 -> Stream Int8
constI8  = constant
constI16 :: Int16 -> Stream Int16
constI16 = constant
constI32 :: Int32 -> Stream Int32
constI32 = constant
constI64 :: Int64 -> Stream Int64
constI64 = constant
constF   :: Float -> Stream Float
constF   = constant
constD   :: Double -> Stream Double
constD   = constant
