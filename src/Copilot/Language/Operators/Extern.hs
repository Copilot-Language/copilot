--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Extern
  ( extern
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
  ) where

import Copilot.Core (Typed)
import Copilot.Language.Stream
import Data.Word
import Data.Int

--------------------------------------------------------------------------------

extern :: Typed a => String -> Stream a
extern = Extern

--------------------------------------------------------------------------------

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
