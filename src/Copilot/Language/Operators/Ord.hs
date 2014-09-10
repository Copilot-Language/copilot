--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Comparison operators.

{-# LANGUAGE Trustworthy #-}

module Copilot.Language.Operators.Ord
  ( (<=)
  , (>=)
  , (<)
  , (>)
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Prelude
import Copilot.Language.Stream
import qualified Prelude as P

--------------------------------------------------------------------------------

(<=) :: (P.Ord a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) <= (Const y) = Const (x P.<= y)
x <= y                 = Op2 (Core.Le typeOf) x y

(>=) :: (P.Ord a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) >= (Const y) = Const (x P.>= y)
x >= y                 = Op2 (Core.Ge typeOf) x y

(<) :: (P.Ord a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) < (Const y) = Const (x P.< y)
x < y                 = Op2 (Core.Lt typeOf) x y

(>) :: (P.Ord a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) > (Const y) = Const (x P.> y)
x > y                 = Op2 (Core.Gt typeOf) x y

--------------------------------------------------------------------------------
