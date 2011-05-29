-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Copilot.Language.Operators.Eq
  ( Eq (..)
  ) where

import qualified Prelude as P
import Copilot.Language.Operators.Boolean
import Copilot.Language.Prelude

class Boolean β ⇒ Eq α β where
  (==) ∷ α → α → β
  (/=) ∷ α → α → β
  x == y = not (x /= y)
  x /= y = not (x == y)

instance P.Eq α ⇒ Eq α Bool where
  (==) = (P.==)
  (/=) = (P./=)
