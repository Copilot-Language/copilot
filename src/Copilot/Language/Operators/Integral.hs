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
div = Op2 (Core.div typeOf)

mod :: (Typed a, P.Integral a) => Stream a -> Stream a -> Stream a
mod = Op2 (Core.mod typeOf)

--------------------------------------------------------------------------------