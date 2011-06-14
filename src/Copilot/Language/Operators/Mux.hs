--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Mux
  ( mux
  , ifThenElse
  ) where

import Copilot.Core (Typed, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Prelude
import Copilot.Language.Stream
import Prelude ()

--------------------------------------------------------------------------------

mux :: Typed a => Stream Bool -> Stream a -> Stream a -> Stream a
mux (Const True) t _  = t
mux (Const False) _ f = f
mux b t f             = Op3 (Core.mux typeOf) b t f

--------------------------------------------------------------------------------

ifThenElse :: Typed a => Stream Bool -> Stream a -> Stream a -> Stream a
ifThenElse = mux

--------------------------------------------------------------------------------
