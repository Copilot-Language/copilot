--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Copilot.Compile.C99.Test.Driver
  ( driver
  ) where

import Copilot.Core
  (Spec (..), Trigger (..), UExpr (..), Type (..), UType (..))
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
          ppUType (UType t) <+> string ("t" ++ show k)

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
ppUType UType { uTypeType = t } = string $ typeSpec' t

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

ppFormat :: UExpr -> Doc
ppFormat
  UExpr { uExprType = t } =
  string $ case t of
    Bool   -> "%d"
    Int8   -> "%d"
    Int16  -> "%d"
    Int32  -> "%d"
    Int64  -> "%lld"
    Word8  -> "%d"
    Word16 -> "%d"
    Word32 -> "%d"
    Word64 -> "%lld"
    Float  -> "%f"
    Double -> "%lf"

--ppExterns :: 
--ppExterns = undefined
