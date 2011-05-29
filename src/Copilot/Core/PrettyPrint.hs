-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | A pretty printer for Copilot specifications.

{-# LANGUAGE Rank2Types #-}

module Copilot.Core.PrettyPrint
  ( prettyPrint
  ) where

import Copilot.Core
import Prelude hiding (abs, drop, and, div, mod, or)
import Text.PrettyPrint.HughesPJ

newtype PPExpr α       = PPExpr { ppExpr :: Doc }
newtype PPOp1  α β     = PPOp1  { ppOp1  :: Doc -> Doc }
newtype PPOp2  α β γ   = PPOp2  { ppOp2  :: Doc -> Doc -> Doc }
newtype PPOp3  α β γ δ = PPOp3  { ppOp3  :: Doc -> Doc -> Doc -> Doc }

instance Expr PPExpr where
  const x         = PPExpr $ text (show x)
  drop i id_      = PPExpr $ text "drop" <+> int i <+> text "s" <> int id_
  extern name     = PPExpr $ text "extern \"" <+> text name <+> text "\""
  op1 op e        = PPExpr $ ppOp1 op (ppExpr e)
  op2 op e1 e2    = PPExpr $ ppOp2 op (ppExpr e1) (ppExpr e2)
  op3 op e1 e2 e3 = PPExpr $ ppOp3 op (ppExpr e1) (ppExpr e2) (ppExpr e3)

instance Op1 PPOp1 where
  not  = PPOp1 $ ppPrefix "not"
  abs  = PPOp1 $ ppPrefix "abs"
  sign = PPOp1 $ ppPrefix "signum"

instance Op2 PPOp2 where
  and  = PPOp2 $ ppInfix "&&"
  or   = PPOp2 $ ppInfix "||"
  add  = PPOp2 $ ppInfix "+"
  sub  = PPOp2 $ ppInfix "-"
  mul  = PPOp2 $ ppInfix "*"
  mod  = PPOp2 $ ppInfix "mod"
  div  = PPOp2 $ ppInfix "mod"
  eq   = PPOp2 $ ppInfix "=="
  ne   = PPOp2 $ ppInfix "/="
  le   = PPOp2 $ ppInfix "<="
  ge   = PPOp2 $ ppInfix ">="
  lt   = PPOp2 $ ppInfix "<"
  gt   = PPOp2 $ ppInfix ">"

instance Op3 PPOp3 where
  mux  = PPOp3 $ \ cs ds es ->
    text "if"    <+> cs <+>
    text "then " <+> ds <+>
    text "else " <+> es

ppInfix :: String -> Doc -> Doc -> Doc
ppInfix cs doc1 doc2 = parens $ doc1 <+> text cs <+> doc2

ppPrefix :: String -> Doc -> Doc
ppPrefix cs = (text cs <+>)

ppStream :: Stream -> Doc
ppStream (Stream id_ buffer _ e) =
  text "s" <> int id_ <+> text (show buffer) <+> text "=" <+> ppExpr e

ppTrigger :: Trigger -> Doc
ppTrigger (Trigger name _ e) =
  text "trigger:" <+> text name <+> text "=" <+> ppExpr e

ppSpec :: Spec -> Doc
ppSpec spec = cs $$ ds
  where
    cs = foldr (($$) . ppStream)  empty (specStreams spec)
    ds = foldr (($$) . ppTrigger) empty (specTriggers spec)

-- | Pretty-prints a CoPilot specification.
prettyPrint :: Spec -> String
prettyPrint = render . ppSpec
