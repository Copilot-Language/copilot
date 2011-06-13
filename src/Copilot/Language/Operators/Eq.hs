--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Copilot.Language.Operators.Eq
  ( Eq (..)
  ) where

import Prelude ()

--------------------------------------------------------------------------------

class Eq a b where
  (==) :: a -> a -> b
  (/=) :: a -> a -> b

--------------------------------------------------------------------------------
