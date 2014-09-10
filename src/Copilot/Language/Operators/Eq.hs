--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Equality operator.

{-# LANGUAGE Trustworthy #-}

module Copilot.Language.Operators.Eq
  ( (==)
  , (/=)
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Prelude
import Copilot.Language.Stream
import qualified Prelude as P

--------------------------------------------------------------------------------

(==) :: (P.Eq a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) == (Const y) = Const (x P.== y)
x == y = Op2 (Core.Eq typeOf) x y

(/=) :: (P.Eq a, Typed a) => Stream a -> Stream a -> Stream Bool
(Const x) /= (Const y) = Const (x P./= y)
x /= y = Op2 (Core.Ne typeOf) x y

--------------------------------------------------------------------------------
