--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.Test.Driver
  ( driver
  ) where

import Copilot.Core
  (Spec (..), Trigger (..), TriggerArg (..), Type (..))
import Data.List (intersperse)
import Data.Text (Text)
import Text.PP
  (Doc, ($$), (<>), (<+>), indent, string, empty, render, concatV, concatH)

driver :: String -> Spec -> Text
driver name Spec { specTriggers = trigs } =
  render $
    ppHeader name $$
    ppMain name $$
    ppTriggers trigs

ppHeader :: String -> Doc
ppHeader name =
  concatH $
    [ string "#include <stdint.h>"
    , string "#include <stdio.h>"
    , string "#include \"" <> string name <> string ".h\""
    ]

ppMain :: String -> Doc
ppMain name =
  concatH $
    [ string "int main(int argc, char const *argv[]) {"
    , string "  int i, k;"
    , string "  k = atoi(argv[1]);"
    , string "  for (i = 0; i < k; i++) {"
    , string "    " <> it <+> it <+> it <+> it <+> it
    , string "    if (i < k-1) printf(\"#\\n\");"
    , string "  }"
    , string "  return 0;"
    , string "}"
    ]

  where

    it :: Doc
    it = string name <> string "();"

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

ppPrintf :: String -> [TriggerArg] -> Doc
ppPrintf name args =
  string "printf(\"" <>
  string name <>
  string "," <>
  ppFormats args <>
  string "\\n\"," <+>
  ppArgs args <>
  string ")"

ppFormats :: [TriggerArg] -> Doc
ppFormats
  = concatV
  . intersperse (string ",")
  . map ppFormat

ppPars :: [TriggerArg] -> Doc
ppPars
  = concatV
  . intersperse (string ", ")
  . map ppPar
  . zip [0..]

  where

  ppPar :: (Int, TriggerArg) -> Doc
  ppPar (k, par) = case par of
    TriggerArg
      { triggerArgType = t } ->
          ppType t <+> string ("t" ++ show k)

ppArgs :: [TriggerArg] -> Doc
ppArgs args
  = concatV
  $ intersperse (string ", ")
  $ map ppArg
  $ [0..length args-1]

  where

  ppArg :: Int -> Doc
  ppArg k = string ("t" ++ show k)

ppType :: Type a -> Doc
ppType t = string $
  case t of
    Bool   _ -> "bool"
    Int8   _ -> "int8_t"   ; Int16  _ -> "int16_t"
    Int32  _ -> "int32_t"  ; Int64  _ -> "int64_t"
    Word8  _ -> "uint8_t"  ; Word16 _ -> "uint16_t"
    Word32 _ -> "uint32_t" ; Word64 _ -> "uint64_t"
    Float  _ -> "float"    ; Double _ -> "double"

ppFormat :: TriggerArg -> Doc
ppFormat
  TriggerArg { triggerArgType = t } =
  string $ case t of
    Bool   _ -> "%d"
    Int8   _ -> "%d" ; Int16  _ -> "%d" ; Int32  _ -> "%d" ; Int64  _ -> "%lld"
    Word8  _ -> "%d" ; Word16 _ -> "%d" ; Word32 _ -> "%d" ; Word64 _ -> "%lld"
    Float  _ -> "%f" ; Double _ -> "%lf"
