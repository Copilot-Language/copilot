--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

module Copilot.Core.StructMarshall
  ( marshalFields
  , demarshalFields
  ) where

import Copilot.Core.Expr (Name, Id, Expr, UExpr)
import Copilot.Core.Type (Type, Typed)
import Copilot.Core.Spec
import Data.BitArray

--------------------------------------------------------------------------------

marshalFields :: [Type] -> BitArray
marshalFields fields =
  foldr buildStruct BitArray fields
  where
    buildStruct :: Type -> BitArray -> BitArray



demarshalFields :: BitArray -> [Type]
demarshalFields struct =
