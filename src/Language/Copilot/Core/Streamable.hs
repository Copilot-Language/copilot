-- |

module Language.Copilot.Core.Streamable (Streamable) where

import Control.DeepSeq (NFData)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Language.Copilot.Core.Array (Array)
import Language.Copilot.Core.Type (Typed)

class
  ( NFData a
  , Eq a
  , Show a
  , Typed a
  ) => Streamable a

instance Streamable Bool
instance Streamable Int8
instance Streamable Int16
instance Streamable Int32
instance Streamable Int64
instance Streamable Word8
instance Streamable Word16
instance Streamable Word32
instance Streamable Word64
instance Streamable Float
instance Streamable Double
instance Streamable a => Streamable (Array a)
