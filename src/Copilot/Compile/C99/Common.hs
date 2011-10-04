--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Copilot.Compile.C99.Common
  ( mkTmpExtFunVarName
  , typeSpec
  ) where

import qualified Copilot.Core as C

--------------------------------------------------------------------------------

mkTmpExtFunVarName :: C.Name -> C.Tag -> String
mkTmpExtFunVarName name tag =  "tmp_ext_fun_" ++ name ++ "_" ++ "_" ++ show tag

--------------------------------------------------------------------------------

typeSpec :: C.Type a -> String
typeSpec C.Bool   = "bool"
typeSpec C.Int8   = "int8_t"
typeSpec C.Int16  = "int16_t"
typeSpec C.Int32  = "int32_t"
typeSpec C.Int64  = "int64_t"
typeSpec C.Word8  = "uint8_t"
typeSpec C.Word16 = "uint16_t"
typeSpec C.Word32 = "uint32_t"
typeSpec C.Word64 = "uint64_t"
typeSpec C.Float  = "float"
typeSpec C.Double = "double"
