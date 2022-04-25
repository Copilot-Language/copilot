-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | Integral class operators applied point-wise on streams.

{-# LANGUAGE Safe #-}

module Copilot.Language.Operators.Integral
  ( div
  , mod
  , (^)
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import qualified Copilot.Language.Error as Err
import Copilot.Language.Operators.BitWise ((.<<.))
import Copilot.Language.Stream

import qualified Data.Bits as B
import qualified Prelude as P
import Data.List (foldl', replicate)

-- | Apply the 'Prelude.div' operation to two streams, point-wise.
div :: (Typed a, P.Integral a) => Stream a -> Stream a -> Stream a
(Const 0) `div` _ = Const 0
_ `div` (Const 0) = Err.badUsage "in div: division by zero."
x `div` (Const 1) = x
x `div` y = Op2 (Core.Div typeOf) x y

-- | Apply the 'Prelude.mod' operation to two streams, point-wise.
mod :: (Typed a, P.Integral a) => Stream a -> Stream a -> Stream a
_         `mod` (Const 0) = Err.badUsage "in mod: division by zero."
(Const 0) `mod` _         = (Const 0)
(Const x) `mod` (Const y) = Const (x `P.mod` y)
x `mod` y = Op2 (Core.Mod typeOf) x y

-- | Apply a limited form of exponentiation (@^@) to two streams, point-wise.
--
-- Either the first stream must be the constant 2, or the second must be a
-- constant stream.
(^) :: (Typed a, Typed b, P.Num a, B.Bits a, P.Integral b)
    => Stream a -> Stream b -> Stream a
(Const 0) ^ (Const 0)  = Const 1
(Const 0) ^ x          = Op3 (Core.Mux typeOf) (Op2 (Core.Eq typeOf) x 0) (1) (0)
(Const 1) ^ _          = Const 1
(Const x) ^ (Const y)  = Const (x P.^ y)
(Const 2) ^ y          = (Const 1) .<<. y
x ^ (Const y)          = foldl' ((P.*)) (Const 1) (replicate (P.fromIntegral y) x)
_ ^ _                  = Err.badUsage "in ^: in x ^ y, either x must be the constant 2, or y must be a constant.  (Do not confuse ^ with bitwise XOR (.^.) or with ** for exponentation of floats/doubles.)"
