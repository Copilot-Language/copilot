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

--data ExternFunArg a = forall b . Typed b => ExternFunArg (a b)

class Extern a where
  extern    :: (Show b, Typed b) => Name -> a b
--  externFun :: Typed b => Name -> [ExternFunArg a] -> a b
--  externArray :: (Integral i, Streamable i, Streamable b) => Name -> a i -> a b

--------------------------------------------------------------------------------