-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Interface.Operators.Ord
  ( Ord (..)
  ) where

import qualified Prelude as P
import Language.Copilot.Interface.Operators.Boolean
import Language.Copilot.Interface.Operators.Eq
import Language.Copilot.Interface.Prelude

class (Boolean β, Eq β α) ⇒ Ord β α where
  (<=) ∷ α → α → β
  (>=) ∷ α → α → β
  (<)  ∷ α → α → β
  (>)  ∷ α → α → β
--  min  ∷ α → α → α
--  max  ∷ α → α → α

instance P.Ord α ⇒ Ord Bool α where
  (<=) = (P.<=)
  (>=) = (P.>=)
  (<)  = (P.<)
  (>)  = (P.>)
--  min  = P.min
--  max  = P.max
