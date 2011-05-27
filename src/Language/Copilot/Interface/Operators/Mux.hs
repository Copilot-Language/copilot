-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Interface.Operators.Mux
  ( Mux (..)
  ) where

import Language.Copilot.Interface.Operators.Boolean

class Boolean β ⇒ Mux β α where
  mux ∷ β → α → α → α

instance Mux Bool α where
  mux v x y = if v then x else y
