-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | A pretty printer for Copilot specifications.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe  #-}

module Copilot.PrettyPrint
  ( prettyPrint
  , ppExpr
  ) where

import Copilot.Core
import Copilot.PrettyPrint.Error (impossible)
import Copilot.PrettyPrint.Type (showWithType, ShowType(..), showType)
import Prelude hiding (id, (<>))
import Text.PrettyPrint.HughesPJ
import Data.List (intersperse)

-- | Create a unique stream name by prefixing the given ID by a lowercase
-- letter @"s"@.
strmName :: Int -> Doc
strmName id = text "s" <> int id

-- | Pretty-print a Copilot expression.
--
-- The type is ignored, and only the expression is pretty-printed.
ppExpr :: Expr a -> Doc
ppExpr e0 = case e0 of
  Const t x                  -> text (showWithType Haskell t x)
  Drop _ 0 id                -> strmName id
  Drop _ i id                -> text "drop" <+> text (show i) <+> strmName id
  ExternVar _ name _         -> text "Ext_" <> (text name)
  Local _ _ name e1 e2       -> text "local" <+> doubleQuotes (text name) <+> equals
                                          <+> ppExpr e1 $$ text "in" <+> ppExpr e2
  Var _ name                 -> text "var" <+> doubleQuotes (text name)
  Op1 op e                   -> ppOp1 op (ppExpr e)
  Op2 op e1 e2               -> ppOp2 op (ppExpr e1) (ppExpr e2)
  Op3 op e1 e2 e3            -> ppOp3 op (ppExpr e1) (ppExpr e2) (ppExpr e3)
  Label _ s e                -> text "label "<> doubleQuotes (text s) <+> (ppExpr e)

-- | Pretty-print an untyped expression.
--
-- The type is ignored, and only the expression is pretty-printed.
ppUExpr :: UExpr -> Doc
ppUExpr UExpr { uExprExpr = e0 } = ppExpr e0

-- | Pretty-print a unary operation.
ppOp1 :: Op1 a b -> Doc -> Doc
ppOp1 op = case op of
  Not                     -> ppPrefix "not"
  Abs _                   -> ppPrefix "abs"
  Sign _                  -> ppPrefix "signum"
  Recip _                 -> ppPrefix "recip"
  Exp _                   -> ppPrefix "exp"
  Sqrt _                  -> ppPrefix "sqrt"
  Log _                   -> ppPrefix "log"
  Sin _                   -> ppPrefix "sin"
  Tan _                   -> ppPrefix "tan"
  Cos _                   -> ppPrefix "cos"
  Asin _                  -> ppPrefix "asin"
  Atan _                  -> ppPrefix "atan"
  Acos _                  -> ppPrefix "acos"
  Sinh _                  -> ppPrefix "sinh"
  Tanh _                  -> ppPrefix "tanh"
  Cosh _                  -> ppPrefix "cosh"
  Asinh _                 -> ppPrefix "asinh"
  Atanh _                 -> ppPrefix "atanh"
  Acosh _                 -> ppPrefix "acosh"
  Ceiling _               -> ppPrefix "ceiling"
  Floor _                 -> ppPrefix "floor"
  BwNot _                 -> ppPrefix "~"
  Cast _ _                -> ppPrefix "(cast)"
  GetField (Struct _) _ f -> \e -> ppInfix "#" e (text $ accessorName f)
  GetField _ _ _          -> impossible "ppOp1" "Copilot.PrettyPrint"

-- | Pretty-print a binary operation.
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
  Atan2    _   -> ppInfix "atan2"
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
  Index    _   -> ppInfix ".!!"
  UpdateField (Struct _) _ f -> \ doc1 doc2 ->
    parens $ doc1 <+> text "##" <+> text (accessorName f) <+> text "=:" <+> doc2
  UpdateField _ _ _ -> impossible "ppOp2" "Copilot.PrettyPrint"

-- | Pretty-print a ternary operation.
ppOp3 :: Op3 a b c d -> Doc -> Doc -> Doc -> Doc
ppOp3 op = case op of
  Mux _    -> \ doc1 doc2 doc3 ->
    text "(if"   <+> doc1 <+>
    text "then" <+> doc2 <+>
    text "else" <+> doc3 <> text ")"

-- | Parenthesize two 'Doc's, separated by an infix 'String'.
ppInfix :: String -> Doc -> Doc -> Doc
ppInfix cs doc1 doc2 = parens $ doc1 <+> text cs <+> doc2

-- | Prefix a 'Doc' by a 'String'.
ppPrefix :: String -> Doc -> Doc
ppPrefix cs = (text cs <+>)

-- | Pretty-print a Copilot stream as a case of a top-level function for
-- streams of that type, by pattern matching on the stream name.
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

-- | Pretty-print a Copilot trigger as a case of a top-level @trigger@
-- function, by pattern matching on the trigger name.
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

-- | Pretty-print a Copilot observer as a case of a top-level @observer@
-- function, by pattern matching on the observer name.
ppObserver :: Observer -> Doc
ppObserver
  Observer
    { observerName     = name
    , observerExpr     = e }
  =   text "observer \"" <> text name <> text "\""
  <+> text "="
  <+> ppExpr e

-- | Pretty-print a Copilot property as a case of a top-level @property@
-- function, by pattern matching on the property name.
ppProperty :: Property -> Doc
ppProperty
  Property
    { propertyName     = name
    , propertyExpr     = e }
  =   text "property \"" <> text name <> text "\""
  <+> text "="
  <+> ppExpr e

-- | Pretty-print a Copilot specification, in the following order:
--
-- - Streams definitions
-- - Trigger definitions
-- - Observer definitions
-- - Property definitions
ppSpec :: Spec -> Doc
ppSpec spec = cs $$ ds $$ es $$ fs
  where
    cs = foldr (($$) . ppStream)   empty (specStreams   spec)
    ds = foldr (($$) . ppTrigger)  empty (specTriggers  spec)
    es = foldr (($$) . ppObserver) empty (specObservers spec)
    fs = foldr (($$) . ppProperty) empty (specProperties spec)

-- | Pretty-print a Copilot specification.
prettyPrint :: Spec -> String
prettyPrint = render . ppSpec
