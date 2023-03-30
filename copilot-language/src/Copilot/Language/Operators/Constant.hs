-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

{-# LANGUAGE Safe #-}

-- | Primitives to build constant streams.
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
import GHC.Stack (HasCallStack)

-- | Create a constant stream that is equal to the given argument, at any
-- point in time.
constant :: (HasCallStack, Typed a) => a -> Stream a
constant = Const

-- | Create a constant stream carrying values of type 'Bool' that is equal to
-- the given argument, at any point in time.
constB :: HasCallStack => Bool -> Stream Bool
constB = constant

-- | Create a constant stream carrying values of type 'Word8' that is equal to
-- the given argument, at any point in time.
constW8 :: HasCallStack => Word8 -> Stream Word8
constW8 = constant

-- | Create a constant stream carrying values of type 'Word16' that is equal to
-- the given argument, at any point in time.
constW16 :: HasCallStack => Word16 -> Stream Word16
constW16 = constant

-- | Create a constant stream carrying values of type 'Word32' that is equal to
-- the given argument, at any point in time.
constW32 :: HasCallStack => Word32 -> Stream Word32
constW32 = constant

-- | Create a constant stream carrying values of type 'Word64' that is equal to
-- the given argument, at any point in time.
constW64 :: HasCallStack => Word64 -> Stream Word64
constW64 = constant

-- | Create a constant stream carrying values of type 'Int8' that is equal to
-- the given argument, at any point in time.
constI8 :: HasCallStack => Int8 -> Stream Int8
constI8 = constant

-- | Create a constant stream carrying values of type 'Int16' that is equal to
-- the given argument, at any point in time.
constI16 :: HasCallStack => Int16 -> Stream Int16
constI16 = constant

-- | Create a constant stream carrying values of type 'Int32' that is equal to
-- the given argument, at any point in time.
constI32 :: HasCallStack => Int32 -> Stream Int32
constI32 = constant

-- | Create a constant stream carrying values of type 'Int64' that is equal to
-- the given argument, at any point in time.
constI64 :: HasCallStack => Int64 -> Stream Int64
constI64 = constant

-- | Create a constant stream carrying values of type 'Float' that is equal to
-- the given argument, at any point in time.
constF :: HasCallStack => Float -> Stream Float
constF = constant

-- | Create a constant stream carrying values of type 'Double' that is equal to
-- the given argument, at any point in time.
constD :: HasCallStack => Double -> Stream Double
constD = constant
