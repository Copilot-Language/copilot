-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Interface.Operators.Eq
  ( Eq (..)
  ) where

import qualified Prelude as P
import Language.Copilot.Interface.Operators.Boolean
import Language.Copilot.Interface.Prelude

class Boolean β ⇒ Eq β α where
  (==) ∷ α → α → β
  (/=) ∷ α → α → β
  x == y = not (x /= y)
  x /= y = not (x == y)

instance P.Eq α ⇒ Eq Bool α where
  (==) = (P.==)
  (/=) = (P./=)
