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

class Boolean a where
  (&&)     :: a -> a -> a
  (||)     :: a -> a -> a
  not      :: a -> a
  true     :: a
  false    :: a
  fromBool :: Bool -> a

--------------------------------------------------------------------------------

instance Boolean Bool where
  (&&)      = (P.&&)
  (||)      = (P.||)
  not       = P.not
  true      = P.True
  false     = P.False
  fromBool  = P.id

--------------------------------------------------------------------------------

infix 5 && 
infix 5 ||

