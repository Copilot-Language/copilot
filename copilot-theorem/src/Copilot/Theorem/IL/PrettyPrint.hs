{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Safe           #-}

-- | This module implements a pretty printer for the IL format, an intermediate
-- representation used in copilot-theorem to facilitate model checking.
module Copilot.Theorem.IL.PrettyPrint (prettyPrint, printConstraint) where

import Copilot.Theorem.IL.Spec
import Text.PrettyPrint.HughesPJ
import qualified Data.Map as Map

import Prelude hiding (id, (<>))

-- | Pretty print an IL specification.
prettyPrint :: IL -> String
prettyPrint = render . ppSpec

-- | Pretty print an IL constraint expression.
printConstraint :: Expr -> String
printConstraint = render . ppExpr

indent :: Doc -> Doc
indent = nest 4
emptyLine :: Doc
emptyLine = text ""

ppSpec :: IL -> Doc
ppSpec (IL { modelInit, modelRec, properties }) =
  text "MODEL INIT"
  $$ indent (foldr (($$) . ppExpr) empty modelInit) $$ emptyLine
  $$ text "MODEL REC"
  $$ indent (foldr (($$) . ppExpr) empty modelRec) $$ emptyLine
  $$ text "PROPERTIES"
  $$ indent (Map.foldrWithKey (\k -> ($$) . ppProp k)
        empty properties )

ppProp :: PropId -> ([Expr], Expr) -> Doc
ppProp id (as, c) = (foldr (($$) . ppExpr) empty as)
  $$ quotes (text id) <+> colon <+> ppExpr c

ppExpr :: Expr -> Doc
ppExpr (ConstB v) = text . show $ v
ppExpr (ConstR v) = text . show $ v
ppExpr (ConstI _ v) = text . show $ v

ppExpr (Ite _ c e1 e2) =
  text "if" <+> ppExpr c
  <+> text "then" <+> ppExpr e1
  <+> text "else" <+> ppExpr e2

ppExpr (Op1 _ op e) = ppOp1 op <+> ppExpr e

ppExpr (Op2 _ op e1 e2) =
  ppExpr e1 <+> ppOp2 op <+> ppExpr e2

ppExpr (SVal _ s i) = text s <> brackets (ppSeqIndex i)

ppExpr (FunApp _ name args) =
  text name <> parens (hsep . punctuate (comma <> space) $ map ppExpr args)

ppSeqIndex :: SeqIndex -> Doc
ppSeqIndex (Var i)
  | i == 0    = text "n"
  | i < 0     = text "n" <+> text "-" <+> integer (-i)
  | otherwise = text "n" <+> text "+" <+> integer i

ppSeqIndex (Fixed i) = integer i

ppOp1 :: Op1 -> Doc
ppOp1 = text . show

ppOp2 :: Op2 -> Doc
ppOp2 = text . show
