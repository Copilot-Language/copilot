-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

{-# LANGUAGE Safe #-}

-- | Equality applied point-wise on streams.
module Copilot.Language.Operators.Eq
  ( (==)
  , (/=)
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Prelude
import Copilot.Language.Stream
import qualified Prelude as P
import GHC.Stack (HasCallStack)

-- | Compare two streams point-wise for equality.
--
-- The output stream contains the value True at a point in time if both
-- argument streams contain the same value at that point in time.
(==) :: (HasCallStack, P.Eq a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) == (Const y) = Const (x P.== y)
x == y = Op2 (Core.Eq typeOf) x y

-- | Compare two streams point-wise for inequality.
--
-- The output stream contains the value True at a point in time if both
-- argument streams contain different values at that point in time.
(/=) :: (HasCallStack, P.Eq a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) /= (Const y) = Const (x P./= y)
x /= y = Op2 (Core.Ne typeOf) x y
