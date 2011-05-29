-- |

{-# LANGUAGE UnicodeSyntax #-}

module Copilot.Language.Operators.Extern
  ( Extern (..)
  ) where

import Copilot.Core (Name, Streamable)

class Extern α where
  extern ∷ Streamable β ⇒ Name → α β
