-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

{-# LANGUAGE Safe #-}

-- | Comparison operators applied point-wise on streams.
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
import GHC.Stack (HasCallStack)

-- | Compare two streams point-wise for order.
--
-- The output stream contains the value True at a point in time if the
-- element in the first stream is smaller or equal than the element in
-- the second stream at that point in time, and False otherwise.
(<=) :: (HasCallStack, P.Ord a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) <= (Const y) = Const (x P.<= y)
x <= y                 = Op2 (Core.Le typeOf) x y

-- | Compare two streams point-wise for order.
--
-- The output stream contains the value True at a point in time if the
-- element in the first stream is greater or equal than the element in
-- the second stream at that point in time, and False otherwise.
(>=) :: (HasCallStack, P.Ord a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) >= (Const y) = Const (x P.>= y)
x >= y                 = Op2 (Core.Ge typeOf) x y

-- | Compare two streams point-wise for order.
--
-- The output stream contains the value True at a point in time if the
-- element in the first stream is smaller than the element in the second stream
-- at that point in time, and False otherwise.
(<) :: (HasCallStack, P.Ord a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) < (Const y) = Const (x P.< y)
x < y                 = Op2 (Core.Lt typeOf) x y

-- | Compare two streams point-wise for order.
--
-- The output stream contains the value True at a point in time if the element
-- in the first stream is greater than the element in the second stream at that
-- point in time, and False otherwise.
(>) :: (HasCallStack, P.Ord a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) > (Const y) = Const (x P.> y)
x > y                 = Op2 (Core.Gt typeOf) x y
