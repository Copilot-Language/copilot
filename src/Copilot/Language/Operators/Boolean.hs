--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Boolean
  ( (&&)
  , (||)
  , not
  , true
  , false
  ) where

import qualified Copilot.Core as Core
import Copilot.Language.Prelude
import Copilot.Language.Stream
import Prelude ()

--------------------------------------------------------------------------------

(&&) :: Stream Bool -> Stream Bool -> Stream Bool
(&&) = Op2 Core.and

(||) :: Stream Bool -> Stream Bool -> Stream Bool
(||) = Op2 Core.or

not :: Stream Bool -> Stream Bool
not = Op1 Core.not

true :: Stream Bool
true = constant True

false :: Stream Bool
false = constant False

--------------------------------------------------------------------------------