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
import Data.Text (pack)
import Text.PrettyPrint
  (Doc, ($$), (<>), (<+>), nest, text, empty, render, vcat, hcat)

type ExternalEnv = Map String (UType, [Int])

driver :: ExternalEnv -> Int -> Spec -> Text
driver _ numIterations Spec { specTriggers = trigs } =
  pack $ render $
    ppHeader $$
    ppMain numIterations $$
    ppTriggers trigs

ppHeader :: Doc
ppHeader =
  hcat $
    [ text "#include <stdint.h>"
    , text "#include <stdio.h>"
    , text "#include \"copilot.h\""
    ]

ppMain :: Int -> Doc
ppMain numIterations =
  hcat $
    [ text "int main(int argc, char const *argv[]) {"
    , text "  int i, k;"
    , text "  k = " <> text (show numIterations) <> text ";"
    , text "  for (i = 0; i < k; i++) {"
    , text "    " <> it <+> it <+> it <+> it <+> it
    , text "    if (i < k-1) printf(\"#\\n\");"
    , text "  }"
    , text "  return 0;"
    , text "}"
    ]

  where

    it :: Doc
    it = text "step();"

ppTriggers :: [Trigger] -> Doc
ppTriggers = foldr ($$) empty . map ppTrigger

ppTrigger :: Trigger -> Doc
ppTrigger
  Trigger
    { triggerName = name
    , triggerArgs = args } =
  hcat $
    [ text "void" <+>
        text name <+>
        text "(" <>
        ppPars args <>
        text ")"
    , text "{"
    , nest 2 $
        ppPrintf name args <>
        text ";"
    , text "}"
    ]

ppPrintf :: String -> [UExpr] -> Doc
ppPrintf name args =
  text "printf(\"" <>
  text name <>
  text "," <>
  ppFormats args <>
  text "\\n\"," <+>
  ppArgs args <>
  text ")"

ppFormats :: [UExpr] -> Doc
ppFormats
  = vcat
  . intersperse (text ",")
  . map ppFormat

ppPars :: [UExpr] -> Doc
ppPars
  = vcat
  . intersperse (text ", ")
  . map ppPar
  . zip [0..]

  where

  ppPar :: (Int, UExpr) -> Doc
  ppPar (k, par) = case par of
    UExpr
      { uExprType = t } ->
          ppUType (UType t) <+> text ("t" ++ show k)

ppArgs :: [UExpr] -> Doc
ppArgs args
  = vcat
  $ intersperse (text ", ")
  $ map ppArg
  $ [0..length args-1]

  where

  ppArg :: Int -> Doc
  ppArg k = text ("t" ++ show k)

ppUType :: UType -> Doc
ppUType UType { uTypeType = t } = text $ typeSpec' t

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
  text $ case t of
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
