--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Copilot.Language.Operators.Temporal
  ( Temporal (..)
  ) where

import qualified Prelude as P
import Copilot.Language.Prelude

--------------------------------------------------------------------------------

infixr 3 ++

class Temporal a b where
  (++) :: [b] -> a b -> a b
  drop :: Int -> a b -> a b

--------------------------------------------------------------------------------

instance Temporal [] b where
  (++) = (P.++)
  drop = (P.drop)

--------------------------------------------------------------------------------