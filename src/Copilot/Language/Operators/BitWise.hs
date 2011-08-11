module Copilot.Language.Operators.BitWise
  ( (.&.)
  , complement
  , (.|.)
  , (.^.)
  ) where

import Data.Bits

-- Avoid redefinition of the Operators.Boolean xor
(.^.) :: ( Bits a ) => a -> a -> a
(.^.) = xor
