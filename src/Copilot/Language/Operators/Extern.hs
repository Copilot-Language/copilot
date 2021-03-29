--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Primitives to build streams connected to external variables.

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
  , funArg
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream
import Data.Word
import Data.Int

--------------------------------------------------------------------------------

-- | Create a stream populated by an external global variable.
--
-- The Copilot compiler does not check that the type is correct. If the list
-- given as second argument does not constrain the type of the values carried
-- by the stream, this primitive stream building function will match any stream
-- of any type, which is potentially dangerous if the global variable mentioned
-- has a different type. We rely on the compiler used with the generated code
-- to detect type errors of this kind.
extern :: Typed a
       => String    -- ^ Name of the global variable to make accessible.
       -> Maybe [a] -- ^ Values to be used exclusively for testing/simulation.
       -> Stream a
extern = Extern

-- | Deprecated.
funArg :: Typed a => Stream a -> Arg
funArg = Arg

--------------------------------------------------------------------------------

-- | Create a stream carrying values of type Bool, populated by an external
-- global variable.
externB :: String       -- ^ Name of the global variable to make accessible.
        -> Maybe [Bool] -- ^ Values to be used exclusively for
                        -- testing/simulation.
        -> Stream Bool
externB = extern

-- | Create a stream carrying values of type Word8, populated by an external
-- global variable.
externW8 :: String         -- ^ Name of the global variable to make accessible.
         -> Maybe [Word8]  -- ^ Values to be used exclusively for
                           --   testing/simulation.
         -> Stream Word8
externW8 = extern

-- | Create a stream carrying values of type Word16, populated by an external
-- global variable.
externW16 :: String          -- ^ Name of the global variable to make accessible.
          -> Maybe [Word16]  -- ^ Values to be used exclusively for
                             -- testing/simulation.
          -> Stream Word16
externW16 = extern

-- | Create a stream carrying values of type Word32, populated by an external
-- global variable.
externW32 :: String          -- ^ Name of the global variable to make accessible.
          -> Maybe [Word32]  -- ^ Values to be used exclusively for
                             -- testing/simulation.
          -> Stream Word32
externW32 = extern

-- | Create a stream carrying values of type Word64, populated by an external
-- global variable.
externW64 :: String          -- ^ Name of the global variable to make accessible.
          -> Maybe [Word64]  -- ^ Values to be used exclusively for
                             -- testing/simulation.
          -> Stream Word64
externW64 = extern

-- | Create a stream carrying values of type Int8, populated by an external
-- global variable.
externI8 :: String    -- ^ Name of the global variable to make accessible.
         -> Maybe [Int8] -- ^ Values to be used exclusively for testing/simulation.
         -> Stream Int8
externI8 = extern

-- | Create a stream carrying values of type Int16, populated by an external
-- global variable.
externI16 :: String    -- ^ Name of the global variable to make accessible.
          -> Maybe [Int16] -- ^ Values to be used exclusively for testing/simulation.
          -> Stream Int16
externI16 = extern

-- | Create a stream carrying values of type Int32, populated by an external
-- global variable.
externI32 :: String    -- ^ Name of the global variable to make accessible.
          -> Maybe [Int32] -- ^ Values to be used exclusively for testing/simulation.
          -> Stream Int32
externI32 = extern

-- | Create a stream carrying values of type Int64, populated by an external
-- global variable.
externI64 :: String    -- ^ Name of the global variable to make accessible.
          -> Maybe [Int64] -- ^ Values to be used exclusively for testing/simulation.
          -> Stream Int64
externI64 = extern

-- | Create a stream carrying values of type Double, populated by an external
-- global variable.
externD :: String    -- ^ Name of the global variable to make accessible.
        -> Maybe [Double] -- ^ Values to be used exclusively for testing/simulation.
        -> Stream Double
externD = extern
