---------------------------------------------------------------------------------

module Copilot.Kind.IL.PrettyPrint (prettyPrint, printConstraint) where

import Copilot.Kind.IL.Spec
import Text.PrettyPrint.HughesPJ
import qualified Data.Map as Map

--------------------------------------------------------------------------------

prettyPrint :: Spec -> String
prettyPrint = render . ppSpec

printConstraint :: Constraint -> String
printConstraint = render . ppExpr

indent = nest 4
emptyLine = text ""

ppSpec :: Spec -> Doc
ppSpec
  (Spec
  { modelInit
  , modelRec
  , properties
  }) =

     text "MODEL INIT"
  $$ indent (foldr (($$) . ppExpr) empty modelInit) $$ emptyLine
  $$ text "MODEL REC"
  $$ indent (foldr (($$) . ppExpr) empty modelRec) $$ emptyLine
  $$ text "PROPERTIES"
  $$ indent (Map.foldrWithKey (\k -> ($$) . ppProp k)
        empty properties )

ppProp :: PropId -> Constraint -> Doc
ppProp id c = quotes (text id) <+> colon <+> ppExpr c

ppSeqDescr :: SeqDescr -> Doc
ppSeqDescr (SeqDescr id ty) = text id <+> colon <+> ppType ty

ppVarDescr :: VarDescr -> Doc
ppVarDescr (VarDescr id ret) =
  text id
  <+> colon
  <+> text "()"
  <+> text "->"
  <+> ppType ret

ppType :: Type t -> Doc
ppType = text . show

ppUType :: U Type -> Doc
ppUType (U t) = ppType t

ppUExpr :: U Expr -> Doc
ppUExpr (U e) = ppExpr e

ppExpr :: Expr t -> Doc
ppExpr (Const Integer v) = text . show $ v
ppExpr (Const Bool    v) = text . show $ v
ppExpr (Const Real    v) = text . show $ v

ppExpr (Ite _ c e1 e2) =
  text "if" <+> ppExpr c
  <+> text "then" <+> ppExpr e1
  <+> text "else" <+> ppExpr e2

ppExpr (Op1 _ op e) = ppOp1 op <+> ppExpr e

ppExpr (Op2 _ op e1 e2) =
  ppExpr e1 <+> ppOp2 op <+> ppExpr e2

ppExpr (SVal _ s i) = text s <> brackets (ppSeqIndex i)

ppExpr (FunApp _ name args) =
  text name <> parens (hsep . punctuate (comma <> space) $ map ppUExpr args)

ppSeqIndex :: SeqIndex -> Doc
ppSeqIndex (Var i)
  | i == 0    = text "n"
  | i < 0     = text "n" <+> text "-" <+> integer (-i)
  | otherwise = text "n" <+> text "+" <+> integer i

ppSeqIndex (Fixed i) = integer i

ppOp1 :: Op1 a b -> Doc
ppOp1 = text . show

ppOp2 :: Op2 a b c -> Doc
ppOp2 = text . show

--------------------------------------------------------------------------------
