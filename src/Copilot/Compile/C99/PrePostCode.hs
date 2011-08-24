--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

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

  typeSpec' Bool   = "bool"
  typeSpec' Int8   = "int8_t"
  typeSpec' Int16  = "int16_t"
  typeSpec' Int32  = "int32_t"
  typeSpec' Int64  = "int64_t"
  typeSpec' Word8  = "uint8_t"
  typeSpec' Word16 = "uint16_t"
  typeSpec' Word32 = "uint32_t"
  typeSpec' Word64 = "uint64_t"
  typeSpec' Float  = "float"
  typeSpec' Double = "double"

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
