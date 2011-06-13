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
import Copilot.Language.Operators.Constant (constant)
import Copilot.Language.Stream
import Prelude ()

--------------------------------------------------------------------------------

infix 5 && 

(&&) :: Stream Bool -> Stream Bool -> Stream Bool
(&&) = Op2 Core.and

infix 5 ||

(||) :: Stream Bool -> Stream Bool -> Stream Bool
(||) = Op2 Core.or

not :: Stream Bool -> Stream Bool
not = Op1 Core.not

true :: Stream Bool
true = constant True

false :: Stream Bool
false = constant False

--------------------------------------------------------------------------------