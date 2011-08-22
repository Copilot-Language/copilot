--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.PrePostCode
  ( preCode , postCode
  ) where

import Copilot.Core
import Copilot.Compile.C99.Params
import Copilot.Compile.C99.Phases (numberOfPhases)
import Copilot.Compile.Header.C99 (c99HeaderName)

--------------------------------------------------------------------------------

preCode :: Params -> Spec -> String
preCode params spec = unlines $
  [ "#include \"" ++ c99HeaderName (prefix params) ++ "\"" ] ++
  ( map (observerDecl params) . specObservers ) spec

--------------------------------------------------------------------------------

observerDecl :: Params -> Observer -> String
observerDecl params (Observer cs _ t) = typeSpec (UType t) ++ " " ++ name ++ ";"

  where

    name = withPrefix (prefix params) cs

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

postCode :: Params -> Spec -> String
postCode params _ =
  unlines
    [ "void " ++ withPrefix (prefix params) "step" ++ "()"
    , "{"
    , "  " ++ concat (replicate numberOfPhases step)
    , "}"
    ]

  where

  step = withPrefix (prefix params) "copilot" ++ "();"
