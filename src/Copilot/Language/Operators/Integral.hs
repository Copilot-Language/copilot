--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Integral
  ( div
  , mod
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Stream
import qualified Prelude as P

--------------------------------------------------------------------------------

div :: (Typed a, P.Integral a) => Stream a -> Stream a -> Stream a
(Const 0) `div` _ = Const 0
_ `div` (Const 0) = P.error "Error in div: division by zero."
x `div` (Const 1) = x
x `div` y = Op2 (Core.div typeOf) x y

mod :: (Typed a, P.Integral a) => Stream a -> Stream a -> Stream a
(Const 0) `mod` _         = (Const 0)
(Const 1) `mod` (Const 1) = (Const 0)
(Const 1) `mod` _         = (Const 1)
(Const x) `mod` (Const y) = Const (x `P.mod` y)
x `mod` y = Op2 (Core.mod typeOf) x y

--------------------------------------------------------------------------------
