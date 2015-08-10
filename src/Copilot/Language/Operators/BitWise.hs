--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Bitwise operators.

{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Data.Bits

instance (Typed a, Bits a) => Bits (Stream a) where
  (.&.)        = Op2 (Core.BwAnd typeOf)
  complement   = Op1 (Core.BwNot typeOf)
  (.|.)        = Op2 (Core.BwOr  typeOf)
  xor          = Op2 (Core.BwXor typeOf)
  shiftL       = P.error "shiftL undefined, for left-shifting use .<<."
  shiftR       = P.error "shiftR undefined, for right-shifting use .>>."
  rotate       = P.error "tbd: rotate"
  bitSize      = P.error "tbd: bitSize"
  isSigned     = P.error "tbd: issigned"
  bitSizeMaybe = P.error "tbd: bitSizeMaybe"
  testBit      = P.error "tbd: testBit"
  bit          = P.error "tbd: bit"
  popCount     = P.error "tbd: popCount"

-- Avoid redefinition of the Operators.Boolean xor
(.^.) :: Bits a => a -> a -> a
(.^.) = xor

(.<<.), (.>>.) :: (Bits a, Typed a, Typed b, P.Integral b)
               => Stream a -> Stream b -> Stream a
(.<<.) = Op2 (Core.BwShiftL typeOf typeOf)
(.>>.) = Op2 (Core.BwShiftR typeOf typeOf)
