--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.PreCode
  ( preCode
  ) where

import qualified Copilot.Core as Core
import Copilot.Core.Spec.Externals (Extern (..), externals)

--------------------------------------------------------------------------------

preCode :: Core.Spec -> String
preCode = unlines . map externProto . externals

--------------------------------------------------------------------------------

externProto :: Extern -> String
externProto (Extern name t) = "extern " ++ typeSpec t ++ " " ++ name ++ ";"

--------------------------------------------------------------------------------

typeSpec :: Core.Type a -> String
typeSpec (Core.Bool   _) = "bool"
typeSpec (Core.Int8   _) = "int8_t"
typeSpec (Core.Int16  _) = "int16_t"
typeSpec (Core.Int32  _) = "int32_t"
typeSpec (Core.Int64  _) = "int64_t"
typeSpec (Core.Word8  _) = "uint8_t"
typeSpec (Core.Word16 _) = "uint16_t"
typeSpec (Core.Word32 _) = "uint32_t"
typeSpec (Core.Word64 _) = "uint64_t"
typeSpec (Core.Float  _) = "float"
typeSpec (Core.Double _) = "double"

--------------------------------------------------------------------------------
