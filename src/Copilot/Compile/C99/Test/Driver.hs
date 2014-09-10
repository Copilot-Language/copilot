--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Compile.C99.Test.Driver
  ( driver
--  , ExtVars
  ) where

import Copilot.Core
  ( Spec(..), Trigger(..), UExpr(..), Type(..), UType(..), Name
  , Expr(..), Stream(..)
  )


import Copilot.Core.Error (impossible)
import Copilot.Core.Type (Typed, typeOf)
import Copilot.Core.Type.Dynamic (DynamicF (..), toDynF)
import Copilot.Core.Type.Show (showWithType, ShowType (..))
--import Copilot.Core.Interpret.Eval (ExtEnv(..))

import Data.List (intersperse, nubBy)
import Data.Text (Text)
import Data.Text (pack)
import Text.PrettyPrint
  ( Doc, ($$), (<>), (<+>), nest, text, empty, render, vcat
  , hcat, space, equals, semi, lbrack, rbrack, comma
  , punctuate, hsep, lbrace, rbrace)

--------------------------------------------------------------------------------

driver :: Int -> Spec -> Text
driver numIterations Spec { specTriggers = trigs 
                          , specStreams = streams } 
  =
  pack . render $
    ppHeader $$
    ppEnvDecls numIterations env $$
    ppMain numIterations env $$
    ppTriggers trigs
  where 
  env :: [ExtVars]
  env = nubBy (\(name0,_) (name1,_) -> name0 == name1) 
              (concatMap extractExts exprs)
  exprs =    map strmExpr streams
          ++ concatMap triggerArgs trigs 
          ++ map uexpr (map triggerGuard trigs)
  strmExpr Stream { streamExpr = expr } = uexpr expr
  uexpr :: Typed a => Expr a -> UExpr
  uexpr e = UExpr { uExprType = typeOf
                  , uExprExpr = e }

--------------------------------------------------------------------------------

-- XXX will need to be generalized to external arrays and functions at some
-- point.
type  ExtVars = (Name, DynamicF [] Type)

extractExts :: UExpr -> [ExtVars]
extractExts UExpr { uExprExpr = expr } 
  = go expr

  where
  go :: Expr a -> [ExtVars]
  go e =
    case e of
      Const _ _                    -> []
      Drop _ _ _                   -> []
      Local _ _ _ e0 e1            -> go e0 ++ go e1
      Var _ _                      -> []
      ExternVar t name mvals       -> 
        case mvals of
          Nothing   -> impossible "extractExts" "copilot-c99/Test" 
          Just vals -> [(name, toDynF t vals)]
      ExternFun _ _ es _ _         -> concatMap extractExts es
      ExternArray _ _ _ _ idx _ _  -> go idx
      Op1 _ e0                     -> go e0
      Op2 _ e0 e1                  -> go e0 ++ go e1
      Op3 _ e0 e1 e2               -> go e0 ++ go e1 ++ go e2

--------------------------------------------------------------------------------

ppHeader :: Doc
ppHeader =
  vcat $
    [ text "#include <stdint.h>"
    , text "#include <inttypes.h>"
    , text "#include <stdio.h>"
    , text "#include \"copilot.h\""
    ]

--------------------------------------------------------------------------------

ppEnvDecls :: Int -> [ExtVars] -> Doc
ppEnvDecls numIterations vars = 
  vcat $  
    [ space  
    , text "// External variables" 
    , vcat $ map ppEnvDecl vars
    , space
    , text "// External values" 
    , vcat $ map ppVals vars
    , space
    , space
    ]

  where
  ppEnvDecl :: (Name, DynamicF [] Type) -> Doc
  ppEnvDecl (name, DynamicF _ t) = 
    cType <+> text name <> semi
    where
    cType = ppUType UType { uTypeType = t }

  ppVals :: (Name, DynamicF [] Type) -> Doc
  ppVals (name, DynamicF vals t) = 
    cType <+> valsName name 
      <> lbrack <> (text . show) numIterations <> rbrack 
      <+> equals <+> lbrace <+> arrVals <+> rbrace <> semi
    where
    cType = ppUType UType { uTypeType = t }
    arrVals = hsep $ punctuate comma $ map text showVals
    showVals = map (showWithType C t) vals

--------------------------------------------------------------------------------

valsName :: Name -> Doc
valsName name = text name <> text "_vals" 

--------------------------------------------------------------------------------

ppMain :: Int -> [ExtVars] -> Doc
ppMain numIterations vars =
  vcat $
    [ text "int main(int argc, char const *argv[]) {"
    , text "  int i;"
    , text "  for (i = 0; i < " <> rnds <> text "; i++) {"
    , space
    , vcat $ map (nest 3) $ map assignVals vars
    , space
    , text "    if (i > 0) printf(\"#\\n\");"
    , text "    " <> text "step();"
    , text "  }"
    , text "  return 0;"
    , text "}"
    , space
    ]

  where
  rnds = text $ show numIterations
  assignVals :: (Name, DynamicF [] Type) -> Doc
  assignVals (name, _) = 
    text name <+> equals <+> 
      valsName name <> lbrack <> text "i" <> rbrack <> semi

--------------------------------------------------------------------------------

ppTriggers :: [Trigger] -> Doc
ppTriggers = foldr ($$) empty . map ppTrigger

ppTrigger :: Trigger -> Doc
ppTrigger Trigger { triggerName = name
                  , triggerArgs = args } 
  =
  hcat $
    [ text "void" <+> text name <+> 
           text "(" <> ppPars args <> text ")"
    , text "{"
    , nest 2 $ ppPrintf name args <> text ";"
    , text "}"
    ]

--------------------------------------------------------------------------------

ppPrintf :: String -> [UExpr] -> Doc
ppPrintf name args =
  text "printf(\"" <>
  text name <>
  text "," <>
  ppFormats args <>
  text "\"\\n\"," <+>
  ppArgs args <>
  text ")"

--------------------------------------------------------------------------------

ppFormats :: [UExpr] -> Doc
ppFormats
  = vcat
  . intersperse (text ",")
  . map (text "%\"" <>)
  . map ppFormat

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

ppArgs :: [UExpr] -> Doc
ppArgs args
  = vcat
  $ intersperse (text ", ")
  $ map ppArg
  $ [0..length args-1]

  where

  ppArg :: Int -> Doc
  ppArg k = text ("t" ++ show k)

--------------------------------------------------------------------------------

ppUType :: UType -> Doc
ppUType UType { uTypeType = t } = text $ typeSpec' t

  where
  typeSpec' :: Type t -> String
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

ppFormat :: UExpr -> Doc
ppFormat
  UExpr { uExprType = t } =
  text $ case t of
    Bool   -> "PRIu8"
    Int8   -> "PRIi8"
    Int16  -> "PRIi16"
    Int32  -> "PRIi32"
    Int64  -> "PRIi64"
    Word8  -> "PRIu8"
    Word16 -> "PRIu16"
    Word32 -> "PRIu32"
    Word64 -> "PRIu64"
    Float  -> "\"f\""
    Double -> "\"lf\""

--------------------------------------------------------------------------------

