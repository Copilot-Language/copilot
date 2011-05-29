-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Copilot.Language.Operators.Temporal
  ( Temporal (..)
  ) where

import qualified Prelude as P
import Copilot.Language.Prelude

infixr 3 ++

class Temporal α β where
  (++) ∷ [β] → α β → α β
  drop ∷ Int → α β → α β

instance Temporal [] β where
  (++) = (P.++)
  drop = (P.drop)
