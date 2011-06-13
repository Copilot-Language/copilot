--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

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
(==) = Op2 (Core.eq typeOf)

(/=) :: (P.Eq a, Typed a) => Stream a -> Stream a -> Stream Bool
(/=) = Op2 (Core.ne typeOf)

--------------------------------------------------------------------------------