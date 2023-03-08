-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Bitwise operators applied on streams, pointwise.
module Copilot.Language.Operators.BitWise
  ( Bits ((.&.), complement, (.|.))
  , (.^.)
  , (.<<.)
  , (.>>.)
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Stream
import qualified Prelude as P

#if MIN_VERSION_base(4,17,0)
import Data.Bits hiding ((.>>.), (.<<.))
#else
import Data.Bits
#endif

-- | Instance of the 'Bits' class for 'Stream's.
--
-- Only the methods '.&.', 'complement', '.|.' and 'xor' are defined.
instance (Typed a, Bits a) => Bits (Stream a) where
  (.&.)        = Op2 (Core.BwAnd typeOf)
  complement   = Op1 (Core.BwNot typeOf)
  (.|.)        = Op2 (Core.BwOr  typeOf)
  xor          = Op2 (Core.BwXor typeOf)
  shiftL       = P.error "shiftL undefined, for left-shifting use .<<."
  shiftR       = P.error "shiftR undefined, for right-shifting use .>>."
  rotate       = P.error "tbd: rotate"
  bitSize      = P.error "tbd: bitSize"
  bitSizeMaybe = P.error "tbd: bitSizeMaybe"
  isSigned     = P.error "tbd: issigned"
  testBit      = P.error "tbd: testBit"
  bit          = P.error "tbd: bit"
  popCount     = P.error "tbd: popCount"

#if !MIN_VERSION_base(4,17,0)
-- | See 'xor'.
(.^.) :: Bits a => a -> a -> a
(.^.) = xor -- Avoid redefinition of the Operators.Boolean xor
#endif

-- | Shifting values of a stream to the left.
(.<<.) :: (Bits a, Typed a, Typed b, P.Integral b)
       => Stream a -> Stream b -> Stream a
(.<<.) = Op2 (Core.BwShiftL typeOf typeOf)

-- | Shifting values of a stream to the right.
(.>>.) :: (Bits a, Typed a, Typed b, P.Integral b)
       => Stream a -> Stream b -> Stream a
(.>>.) = Op2 (Core.BwShiftR typeOf typeOf)
