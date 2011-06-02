--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language.Operators.Boolean
  ( Boolean (..)
  ) where

import qualified Prelude as P
import Copilot.Language.Prelude

--------------------------------------------------------------------------------

class Boolean α where
  (&&)     :: α -> α -> α
  (||)     :: α -> α -> α
  not      :: α -> α
  true     :: α
  false    :: α
  fromBool :: Bool -> α

--------------------------------------------------------------------------------

instance Boolean Bool where
  (&&)      = (P.&&)
  (||)      = (P.||)
  not       = P.not
  true      = P.True
  false     = P.False
  fromBool  = P.id

--------------------------------------------------------------------------------