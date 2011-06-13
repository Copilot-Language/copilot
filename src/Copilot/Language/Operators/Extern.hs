--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Extern
  ( Extern (..)
  ) where

import Copilot.Core (Name, Typed)

--------------------------------------------------------------------------------

class Extern a where
  extern :: (Show b, Typed b) => Name -> a b

--------------------------------------------------------------------------------
