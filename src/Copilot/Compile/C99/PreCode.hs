--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.PreCode
  ( preCode
  ) where

import Copilot.Core

--------------------------------------------------------------------------------

preCode :: Spec -> String
preCode spec =
  ( unlines . map externProto   . externVars    ) spec ++
  ( unlines . map observerProto . specObservers ) spec

--------------------------------------------------------------------------------

externProto :: ExternVar -> String
externProto (ExternVar name t) = "extern " ++ typeSpec t ++ " " ++ name ++ ";"

--------------------------------------------------------------------------------

observerProto :: Observer -> String
observerProto (Observer name _ t) = typeSpec (UType t) ++ " " ++ name ++ ";"

--------------------------------------------------------------------------------

typeSpec :: UType -> String
typeSpec UType { uTypeType = t } = typeSpec' t

  where

  typeSpec' (Bool   _) = "bool"
  typeSpec' (Int8   _) = "int8_t"
  typeSpec' (Int16  _) = "int16_t"
  typeSpec' (Int32  _) = "int32_t"
  typeSpec' (Int64  _) = "int64_t"
  typeSpec' (Word8  _) = "uint8_t"
  typeSpec' (Word16 _) = "uint16_t"
  typeSpec' (Word32 _) = "uint32_t"
  typeSpec' (Word64 _) = "uint64_t"
  typeSpec' (Float  _) = "float"
  typeSpec' (Double _) = "double"

--------------------------------------------------------------------------------
