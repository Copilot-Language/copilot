-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | A pretty printer for Copilot specifications.

{-# LANGUAGE Rank2Types #-}

module Copilot.Core.PrettyPrint
  ( prettyPrint
  ) where

import Copilot.Core
import Prelude hiding (id)
import Text.PrettyPrint.HughesPJ

newtype PPExpr α         = PPExpr { ppExpr :: Doc }
newtype PPOp1  α β       = PPOp1  { ppOp1  :: Doc -> Doc }
newtype PPOp2  α β γ     = PPOp2  { ppOp2  :: Doc -> Doc -> Doc }
newtype PPOp3  α β γ δ   = PPOp3  { ppOp3  :: Doc -> Doc -> Doc -> Doc }

instance Expr PPExpr where
  const _ x          = PPExpr $ text (show x)
  drop _ i id        = PPExpr $ text "drop" <+> text (show i) <+> text "s" <> int id
  extern _ name      = PPExpr $ text "extern \"" <+> text name <+> text "\""
  op1 op e           = PPExpr $ ppOp1 op (ppExpr e)
  op2 op e1 e2       = PPExpr $ ppOp2 op (ppExpr e1) (ppExpr e2)
  op3 op e1 e2 e3    = PPExpr $ ppOp3 op (ppExpr e1) (ppExpr e2) (ppExpr e3)

instance Op1 PPOp1 where
  not      = PPOp1 $ ppPrefix "not"
  abs _    = PPOp1 $ ppPrefix "abs"
  sign _   = PPOp1 $ ppPrefix "signum"

instance Op2 PPOp2 where
  and      = PPOp2 $ ppInfix "&&"
  or       = PPOp2 $ ppInfix "||"
  add _    = PPOp2 $ ppInfix "+"
  sub _    = PPOp2 $ ppInfix "-"
  mul _    = PPOp2 $ ppInfix "*"
  mod _    = PPOp2 $ ppInfix "mod"
  div _    = PPOp2 $ ppInfix "mod"
  eq _     = PPOp2 $ ppInfix "=="
  ne _     = PPOp2 $ ppInfix "/="
  le _     = PPOp2 $ ppInfix "<="
  ge _     = PPOp2 $ ppInfix ">="
  lt _     = PPOp2 $ ppInfix "<"
  gt _     = PPOp2 $ ppInfix ">"

instance Op3 PPOp3 where
  mux _    = PPOp3 $ \ doc1 doc2 doc3 ->
    text "if"    <+> doc1 <+>
    text "then " <+> doc2 <+>
    text "else " <+> doc3

ppInfix :: String -> Doc -> Doc -> Doc
ppInfix cs doc1 doc2 = parens $ doc1 <+> text cs <+> doc2

ppPrefix :: String -> Doc -> Doc
ppPrefix cs = (text cs <+>)

ppStream :: Stream -> Doc
ppStream (Stream _ id buffer _ e) =
  text "s" <> int id <+> text (show buffer) <+> text "=" <+> ppExpr e

ppTrigger :: Trigger -> Doc
ppTrigger (Trigger _ name _ e) =
  text "trigger:" <+> text name <+> text "=" <+> ppExpr e

ppSpec :: Spec -> Doc
ppSpec spec = cs $$ ds
  where
    cs = foldr (($$) . ppStream)  empty (specStreams spec)
    ds = foldr (($$) . ppTrigger) empty (specTriggers spec)

-- | Pretty-prints a Copilot specification.
prettyPrint :: Spec -> String
prettyPrint = render . ppSpec
