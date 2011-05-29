-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Copilot.Language.Operators.Ord
  ( Ord (..)
  ) where

import qualified Prelude as P
import Copilot.Language.Operators.Boolean
import Copilot.Language.Operators.Eq
import Copilot.Language.Prelude

class (Boolean β, Eq α β) ⇒ Ord α β where
  (<=) ∷ α → α → β
  (>=) ∷ α → α → β
  (<)  ∷ α → α → β
  (>)  ∷ α → α → β
--  min  ∷ α → α → α
--  max  ∷ α → α → α

instance P.Ord α ⇒ Ord α Bool where
  (<=) = (P.<=)
  (>=) = (P.>=)
  (<)  = (P.<)
  (>)  = (P.>)
--  min  = P.min
--  max  = P.max
