module Copilot.Language.Operators.BitWise
  ( Bits ( (.&.), complement, (.|.) )
  , (.^.)
  , (.<<.)
  , (.>>.)
  ) where

import Copilot.Core ( Typed, typeOf )
import qualified Copilot.Core as Core
import Copilot.Language.Stream
import qualified Prelude as P
import Data.Bits

instance ( Typed a, Bits a ) => Bits ( Stream a ) where
  (.&.)      = Op2 ( Core.bwAnd typeOf )
  complement = Op1 ( Core.bwNot typeOf )
  (.|.)      = Op2 ( Core.bwOr  typeOf )
  xor        = Op2 ( Core.bwXor typeOf )
  shiftL     = P.error "shiftL undefined, for left-shifting use .<<."
  shiftR     = P.error "shiftR undefined, for right-shifting use .>>."
  rotate     = P.error "tbd: rotate"
  bitSize    = P.error "tbd: bitSize"
  isSigned   = P.error "tbd: issigned"


-- Avoid redefinition of the Operators.Boolean xor
(.^.) :: ( Bits a ) => a -> a -> a
(.^.) = xor


(.<<.), (.>>.) :: ( Bits a, Typed a, Typed b, P.Integral b ) =>
                    Stream a -> Stream b -> Stream a
(.<<.) = Op2 ( Core.bwShiftL typeOf typeOf )
(.>>.) = Op2 ( Core.bwShiftR typeOf typeOf )
