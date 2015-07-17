--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

module Copilot.Core.StructMarshal
  ( marshalFields
  , demarshalField
  ) where

import Copilot.Core.Expr as E
import Copilot.Core.Type (Type, Typed)
import Copilot.Core.Spec
--import Data.BitArray
import Data.Vector.Unboxed hiding (foldr)
--import Data.Serialize

--------------------------------------------------------------------------------

-- | Convert Struct to Bit Vector
marshalFields :: [Type] -> Vector
marshalFields fields =
  foldr cons empty fields

-- | Convert Bit Vector to Struct
{-demarshalFields :: Vector -> [UExpr]
demarshalFields struct =
  let -}

-- | Get Struct Field from Bit Vector
demarshalField :: Vector -> Type -> Int -> UExpr
demarshalField struct t i =
  UExpr { uExprType = t, uExprExpr = Expr struct!i }