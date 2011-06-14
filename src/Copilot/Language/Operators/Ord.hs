--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

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
x <= y                 = Op2 (Core.le typeOf) x y

(>=) :: (P.Ord a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) >= (Const y) = Const (x P.>= y)
x >= y                 = Op2 (Core.ge typeOf) x y

(<) :: (P.Ord a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) < (Const y) = Const (x P.< y)
x < y                 = Op2 (Core.lt typeOf) x y

(>) :: (P.Ord a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) > (Const y) = Const (x P.> y)
x > y                 = Op2 (Core.gt typeOf) x y

--------------------------------------------------------------------------------
