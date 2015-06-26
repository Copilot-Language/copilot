--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | A pretty printer for Copilot specifications.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}

module Copilot.Core.PrettyPrint
  ( prettyPrint
  , ppExpr
  ) where

import Copilot.Core
import Copilot.Core.Type.Show (showWithType, ShowType(..), showType)
import Prelude hiding (id)
import Text.PrettyPrint.HughesPJ
import Data.List (intersperse)

--------------------------------------------------------------------------------

strmName :: Int -> Doc
strmName id = text "s" <> int id

--------------------------------------------------------------------------------

ppExpr :: Expr a -> Doc
ppExpr e0 = case e0 of
  Const t x                  -> text (showWithType Haskell t x)
  Drop _ 0 id                -> strmName id
  Drop _ i id                -> text "drop" <+> text (show i) <+> strmName id
  ExternVar _ name _         -> text "extern" <+> doubleQuotes (text name)
  ExternFun _ name args _ _  -> 
    text "externFun" <+> (doubleQuotes 
      (text name)) <+> lparen <> 
         (hcat (punctuate (comma <> space) (map ppUExpr args))
       <> rparen)
  ExternArray _ _ name 
              _ idx _ _      -> text "externArray" <+> (doubleQuotes (text name)) <+> lbrack 
                                  <> ppExpr idx <> rbrack
  Local _ _ name e1 e2       -> text "local" <+> doubleQuotes (text name) <+> equals
                                          <+> ppExpr e1 $$ text "in" <+> ppExpr e2
  Var _ name                 -> text "var" <+> doubleQuotes (text name)
  Op1 op e                   -> ppOp1 op (ppExpr e)
  Op2 op e1 e2               -> ppOp2 op (ppExpr e1) (ppExpr e2)
  Op3 op e1 e2 e3            -> ppOp3 op (ppExpr e1) (ppExpr e2) (ppExpr e3)

ppUExpr :: UExpr -> Doc
ppUExpr UExpr { uExprExpr = e0 } = ppExpr e0

ppOp1 :: Op1 a b -> Doc -> Doc
ppOp1 op = case op of
  Not      -> ppPrefix "not"
  Abs _    -> ppPrefix "abs"
  Sign _   -> ppPrefix "signum"
  Recip _  -> ppPrefix "recip"
  Exp _    -> ppPrefix "exp"
  Sqrt _   -> ppPrefix "sqrt"
  Log _    -> ppPrefix "log"
  Sin _    -> ppPrefix "sin"
  Tan _    -> ppPrefix "tan"
  Cos _    -> ppPrefix "cos"
  Asin _   -> ppPrefix "asin"
  Atan _   -> ppPrefix "atan"
  Acos _   -> ppPrefix "acos"
  Sinh _   -> ppPrefix "sinh"
  Tanh _   -> ppPrefix "tanh"
  Cosh _   -> ppPrefix "cosh"
  Asinh _  -> ppPrefix "asinh"
  Atanh _  -> ppPrefix "atanh"
  Acosh _  -> ppPrefix "acosh"
  BwNot _  -> ppPrefix "~"
  Cast _ _ -> ppPrefix "(cast)"

ppOp2 :: Op2 a b c -> Doc -> Doc -> Doc
ppOp2 op = case op of
  And          -> ppInfix "&&"
  Or           -> ppInfix "||"
  Add      _   -> ppInfix "+"
  Sub      _   -> ppInfix "-"
  Mul      _   -> ppInfix "*"
  Div      _   -> ppInfix "div"
  Mod      _   -> ppInfix "mod"
  Fdiv     _   -> ppInfix "/"
  Pow      _   -> ppInfix "**"
  Logb     _   -> ppInfix "logBase"
  Eq       _   -> ppInfix "=="
  Ne       _   -> ppInfix "/="
  Le       _   -> ppInfix "<="
  Ge       _   -> ppInfix ">="
  Lt       _   -> ppInfix "<"
  Gt       _   -> ppInfix ">"
  BwAnd    _   -> ppInfix "&"
  BwOr     _   -> ppInfix "|"
  BwXor    _   -> ppInfix "^"
  BwShiftL _ _ -> ppInfix "<<"
  BwShiftR _ _ -> ppInfix ">>"

ppOp3 :: Op3 a b c d -> Doc -> Doc -> Doc -> Doc
ppOp3 op = case op of
  Mux _    -> \ doc1 doc2 doc3 ->
    text "(if"   <+> doc1 <+>
    text "then" <+> doc2 <+>
    text "else" <+> doc3 <> text ")"

--------------------------------------------------------------------------------
  
ppInfix :: String -> Doc -> Doc -> Doc
ppInfix cs doc1 doc2 = parens $ doc1 <+> text cs <+> doc2

ppPrefix :: String -> Doc -> Doc
ppPrefix cs = (text cs <+>)

--------------------------------------------------------------------------------

ppStream :: Stream -> Doc
ppStream
  Stream
    { streamId       = id
    , streamBuffer   = buffer
    , streamExpr     = e
    , streamExprType = t
    }
      = (parens . text . showType) t
          <+> strmName id 
    <+> text "="
    <+> text ("["
              ++ ( concat $ intersperse "," 
                              $ map (showWithType Haskell t) buffer )
              ++ "]")
    <+> text "++"
    <+> ppExpr e

--------------------------------------------------------------------------------

ppTrigger :: Trigger -> Doc
ppTrigger
  Trigger
    { triggerName  = name
    , triggerGuard = e
    , triggerArgs  = args }
  =   text "trigger" <+> text "\"" <> text name <> text "\""
  <+> text "="
  <+> ppExpr e
  <+> lbrack
  $$  (nest 2 $ vcat (punctuate comma $ 
                          map (\a -> text "arg" <+> ppUExpr a) args))
  $$  nest 2 rbrack

--------------------------------------------------------------------------------

ppObserver :: Observer -> Doc
ppObserver
  Observer
    { observerName     = name
    , observerExpr     = e }
  =   text "observer \"" <> text name <> text "\""
  <+> text "="
  <+> ppExpr e

--------------------------------------------------------------------------------

ppProperty :: Property -> Doc
ppProperty
  Property
    { propertyName     = name
    , propertyExpr     = e }
  =   text "property \"" <> text name <> text "\""
  <+> text "="
  <+> ppExpr e

--------------------------------------------------------------------------------

ppSpec :: Spec -> Doc
ppSpec spec = cs $$ ds $$ es $$ fs
  where
    cs = foldr (($$) . ppStream)   empty (specStreams   spec)
    ds = foldr (($$) . ppTrigger)  empty (specTriggers  spec)
    es = foldr (($$) . ppObserver) empty (specObservers spec)
    fs = foldr (($$) . ppProperty) empty (specProperties spec)

--------------------------------------------------------------------------------

-- | Pretty-prints a Copilot specification.
prettyPrint :: Spec -> String
prettyPrint = render . ppSpec

--------------------------------------------------------------------------------
