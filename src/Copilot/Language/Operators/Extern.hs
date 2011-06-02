--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Language.Operators.Extern
  ( Extern (..)
--  , ExternFunArg (..)
  ) where

import Copilot.Core (Name, Typed)

--------------------------------------------------------------------------------

--data ExternFunArg α = forall β . Typed β => ExternFunArg (α β)

class Extern α where
  extern    :: (Show β, Typed β) => Name -> α β
--  externFun :: Typed β => Name -> [ExternFunArg α] -> α β
--  externArray :: (Integral i, Streamable i, Streamable β) => Name -> α i -> α β

--------------------------------------------------------------------------------