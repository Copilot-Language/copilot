--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.PrePostCode
  ( preCode , postCode
  ) where

import Copilot.Core
import Copilot.Compile.C99.Phases (numberOfPhases)
import Copilot.Compile.Header.C99 (c99HeaderName)

--------------------------------------------------------------------------------

preCode :: Name -> Spec -> String
preCode programName spec = unlines $
  [ "#include \"" ++ c99HeaderName programName ++ "\"" ] ++
  ( map observerDecl . specObservers ) spec

--------------------------------------------------------------------------------

observerDecl :: Observer -> String
observerDecl (Observer name _ t) = typeSpec (UType t) ++ " " ++ name ++ ";"

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

postCode :: Name -> Spec -> String
postCode programName _ =
  unlines
    [ "void step_" ++ programName ++ "()"
    , "{"
    , "  " ++ concat (replicate numberOfPhases step)
    , "}"
    ]

  where

  step = programName ++ "();"
