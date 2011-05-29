-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

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

instance P.Ord α ⇒ Ord α Bool where
  (<=) = (P.<=)
  (>=) = (P.>=)
  (<)  = (P.<)
  (>)  = (P.>)
