--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.Test.Driver
  ( driver
  ) where

import Copilot.Core
  (Spec (..), Trigger (..), UExpr (..), Type (..), UType (..), utype)
import Data.List (intersperse)
import Data.Map (Map)
import Data.Text (Text)
import Text.PP
  (Doc, ($$), (<>), (<+>), indent, string, empty, render, concatV, concatH)

type ExternalEnv = Map String (UType, [Int])

driver :: ExternalEnv -> Int -> String -> Spec -> Text
driver _ numIterations pname Spec { specTriggers = trigs } =
  render $
    ppHeader pname $$
    ppMain numIterations pname $$
    ppTriggers trigs

ppHeader :: String -> Doc
ppHeader pname =
  concatH $
    [ string "#include <stdint.h>"
    , string "#include <stdio.h>"
    , string "#include \"" <> string pname <> string ".h\""
    ]

ppMain :: Int -> String -> Doc
ppMain numIterations pname =
  concatH $
    [ string "int main(int argc, char const *argv[]) {"
    , string "  int i, k;"
    , string "  k = " <> string (show numIterations) <> string ";"
    , string "  for (i = 0; i < k; i++) {"
    , string "    " <> it <+> it <+> it <+> it <+> it
    , string "    if (i < k-1) printf(\"#\\n\");"
    , string "  }"
    , string "  return 0;"
    , string "}"
    ]

  where

    it :: Doc
    it = string pname <> string "();"

ppTriggers :: [Trigger] -> Doc
ppTriggers = foldr ($$) empty . map ppTrigger

ppTrigger :: Trigger -> Doc
ppTrigger
  Trigger
    { triggerName = name
    , triggerArgs = args } =
  concatH $
    [ string "void" <+>
        string name <+>
        string "(" <>
        ppPars args <>
        string ")"
    , string "{"
    , indent 2 $
        ppPrintf name args <>
        string ";"
    , string "}"
    ]

ppPrintf :: String -> [UExpr] -> Doc
ppPrintf name args =
  string "printf(\"" <>
  string name <>
  string "," <>
  ppFormats args <>
  string "\\n\"," <+>
  ppArgs args <>
  string ")"

ppFormats :: [UExpr] -> Doc
ppFormats
  = concatV
  . intersperse (string ",")
  . map ppFormat

ppPars :: [UExpr] -> Doc
ppPars
  = concatV
  . intersperse (string ", ")
  . map ppPar
  . zip [0..]

  where

  ppPar :: (Int, UExpr) -> Doc
  ppPar (k, par) = case par of
    UExpr
      { uExprType = t } ->
          ppUType (utype t) <+> string ("t" ++ show k)

ppArgs :: [UExpr] -> Doc
ppArgs args
  = concatV
  $ intersperse (string ", ")
  $ map ppArg
  $ [0..length args-1]

  where

  ppArg :: Int -> Doc
  ppArg k = string ("t" ++ show k)

ppUType :: UType -> Doc
ppUType t = string $
  case t of
    UBool   -> "bool"
    UInt8   -> "int8_t"   ; UInt16  -> "int16_t"
    UInt32  -> "int32_t"  ; UInt64  -> "int64_t"
    UWord8  -> "uint8_t"  ; UWord16 -> "uint16_t"
    UWord32 -> "uint32_t" ; UWord64 -> "uint64_t"
    UFloat  -> "float"    ; UDouble -> "double"

ppFormat :: UExpr -> Doc
ppFormat
  UExpr { uExprType = t } =
  string $ case t of
    Bool   _ -> "%d"
    Int8   _ -> "%d" ; Int16  _ -> "%d" ; Int32  _ -> "%d" ; Int64  _ -> "%lld"
    Word8  _ -> "%d" ; Word16 _ -> "%d" ; Word32 _ -> "%d" ; Word64 _ -> "%lld"
    Float  _ -> "%f" ; Double _ -> "%lf"

--ppExterns :: 
--ppExterns = undefined
