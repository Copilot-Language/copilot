-- |

module Language.Copilot.Streamable (Streamable) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Typeable (Typeable)
import Language.Copilot.Array (Array)
import Language.Copilot.Type (Typed)

class (Eq a, Show a, Typeable a, Typed a) => Streamable a

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
instance Streamable a => Streamable (Maybe a)
instance (Streamable a, Streamable b) => Streamable (a, b)
instance (Streamable a, Streamable b, Streamable c) => Streamable (a, b, c)
instance (Streamable a, Streamable b, Streamable c, Streamable d)
  => Streamable (a, b, c, d)
