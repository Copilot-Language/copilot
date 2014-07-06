---------------------------------------------------------------------------------

module Copilot.Kind.IL.PrettyPrint (printConstraint) where

import Copilot.Kind.IL.Spec
import Text.PrettyPrint.HughesPJ

--------------------------------------------------------------------------------

printConstraint :: Constraint -> String
printConstraint = render . ppExpr

ppExpr :: Expr t -> Doc
ppExpr (Const Integer v) = text . show $ v
ppExpr (Const Bool    v) = text . show $ v

ppExpr (Ite _ c e1 e2) =
  text "if" <+> ppExpr c
  <+> text "then" <+> ppExpr e1
  <+> text "else" <+> ppExpr e2

ppExpr (Op1 _ op e) = ppOp1 op <+> ppExpr e

ppExpr (Op2 _ op e1 e2) =
  (ppExpr e1) <+> ppOp2 op <+> (ppExpr e2)

ppExpr (SVal _ s i) = text s <> brackets (ppSeqIndex i)

ppSeqIndex :: SeqIndex -> Doc
ppSeqIndex (Var i)
  | i == 0    = text "n"
  | i < 0     = text "n" <+> text "-" <+> integer (-i)
  | otherwise = text "n" <+> text "+" <+> integer i

ppSeqIndex (Fixed i) = integer i


ppOp1 :: Op1 a b -> Doc
ppOp1 op = case op of
  Neg -> text "-"
  Not -> text "not"

ppOp2 :: Op2 a b c -> Doc
ppOp2 op = case op of
  EqB -> text "="
  EqI -> text "="
  Le  -> text "<="
  Lt  -> text "<"
  Ge  -> text ">="
  Gt  -> text ">"

  And -> text "and"
  Or  -> text "or"
  Add -> text "+"
  Mul -> text "*"
  Sub -> text "-"

--------------------------------------------------------------------------------
