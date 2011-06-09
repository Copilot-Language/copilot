--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

module Copilot.Language.Operators.Mux
  ( Mux (..)
  , ifThenElse
  ) where

import Copilot.Language.Operators.Boolean
import qualified Prelude as P

--------------------------------------------------------------------------------

class Boolean b => Mux a b where
  mux :: b -> a -> a -> a

--------------------------------------------------------------------------------

instance Mux a P.Bool where
  mux v x y = if v then x else y

--------------------------------------------------------------------------------

ifThenElse :: Mux a b => b -> a -> a -> a
ifThenElse = mux

--------------------------------------------------------------------------------