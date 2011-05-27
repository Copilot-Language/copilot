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
