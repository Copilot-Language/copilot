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

--------------------------------------------------------------------------------

class Mux a b where
  mux :: b -> a -> a -> a

--------------------------------------------------------------------------------

ifThenElse :: Mux a b => b -> a -> a -> a
ifThenElse = mux

--------------------------------------------------------------------------------