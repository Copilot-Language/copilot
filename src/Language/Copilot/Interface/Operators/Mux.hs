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
