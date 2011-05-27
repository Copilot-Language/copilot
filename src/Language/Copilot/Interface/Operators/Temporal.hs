-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Interface.Operators.Temporal
  ( Temporal (..)
  ) where

import qualified Prelude as P
import Language.Copilot.Interface.Prelude

infixr 3 ++

class Temporal β α | α → β where
  (++) ∷ [β] → α β → α β
  drop ∷ Int → α β → α β

instance Temporal β [] where
  (++) = (P.++)
  drop = (P.drop)
