--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Copilot.Language.Operators.Mux
  ( Mux (..)
  ) where

import Copilot.Language.Operators.Boolean

--------------------------------------------------------------------------------

class Boolean β => Mux α β where
  mux :: β -> α -> α -> α

--------------------------------------------------------------------------------

instance Mux α Bool where
  mux v x y = if v then x else y

--------------------------------------------------------------------------------