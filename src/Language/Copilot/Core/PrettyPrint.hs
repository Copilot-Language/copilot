-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | A pretty printer for Copilot specifications.

{-# LANGUAGE Rank2Types #-}

module Language.Copilot.Core.PrettyPrint
  ( prettyPrint
  ) where

import Language.Copilot.Core
import qualified Language.Copilot.Core.HeteroMap as H
import Language.Copilot.Core.PrettyPrint.HughesPJ

--

newtype PPExpr α       = PPExpr { ppExpr :: Doc }
newtype PPOp1  α β     = PPOp1  { ppOp1  :: Doc -> Doc }
newtype PPOp2  α β γ   = PPOp2  { ppOp2  :: Doc -> Doc -> Doc }
newtype PPOp3  α β γ δ = PPOp3  { ppOp3  :: Doc -> Doc -> Doc -> Doc }

--

instance Expr PPExpr where
  drp i key      = PPExpr $ text "drop" <+> int i <+> text "s" <> ppKey key
  lit x          = PPExpr $ text (show x)
  ext name       = PPExpr $ text "extern \"" <+> text name <+> text "\""
  op1 θ η        = PPExpr $ ppOp1 θ (ppExpr η)
  op2 θ η1 η2    = PPExpr $ ppOp2 θ (ppExpr η1) (ppExpr η2)
  op3 θ η1 η2 η3 = PPExpr $ ppOp3 θ (ppExpr η1) (ppExpr η2) (ppExpr η3)

instance Op1 PPOp1 where
  not'    = PPOp1 $ \ cs -> text "not" <+> cs
  abs'    = PPOp1 $ \ cs -> text "abs" <+> cs
  signum' = PPOp1 $ \ cs -> text "signum" <+> cs

instance Op2 PPOp2 where
  (&&.) = PPOp2 $ \ cs ds -> parens $ cs <+> text "&&"  <+> ds
  (||.) = PPOp2 $ \ cs ds -> parens $ cs <+> text "||"  <+> ds
  (+.)  = PPOp2 $ \ cs ds -> parens $ cs <+> text "+"   <+> ds
  (-.)  = PPOp2 $ \ cs ds -> parens $ cs <+> text "-"   <+> ds
  (*.)  = PPOp2 $ \ cs ds -> parens $ cs <+> text "*"   <+> ds
  mod'  = PPOp2 $ \ cs ds -> parens $ cs <+> text "mod" <+> ds  
  (==.) = PPOp2 $ \ cs ds -> parens $ cs <+> text "=="  <+> ds
  (/=.) = PPOp2 $ \ cs ds -> parens $ cs <+> text "/="  <+> ds
  (<=.) = PPOp2 $ \ cs ds -> parens $ cs <+> text "<="  <+> ds
  (>=.) = PPOp2 $ \ cs ds -> parens $ cs <+> text ">="  <+> ds
  (<.)  = PPOp2 $ \ cs ds -> parens $ cs <+> text "<"   <+> ds
  (>.)  = PPOp2 $ \ cs ds -> parens $ cs <+> text ">"   <+> ds

instance Op3 PPOp3 where
  if_then_else = PPOp3 $ \ cs ds es ->
      text "if" <+> cs <+> text "then " <+> ds <+> text "else " <+> es

--

ppKey :: H.Key -> Doc
ppKey = text . show . H.keyToInt

--

ppStrm :: H.Key -> Strm α -> Doc
ppStrm k (Strm buffer η) =
  text "s" <> ppKey k <+> text (show buffer) <+> text "=" <+> ppExpr η

ppTrig :: Trig -> Doc
ppTrig (Trig name _ η2) =
  text "trigger:" <+> text name <+> text "=" <+> ppExpr η2

ppSpec :: Spec -> Doc
ppSpec (Spec streams triggers) = cs $$ ds
  where
    cs = H.foldWithKey step empty streams
      where
        step k strm acc = acc $$ ppStrm k strm
    ds = foldr step empty triggers
      where
        step trig acc = acc $$ ppTrig trig

-- | Pretty-prints a CoPilot specification.
prettyPrint :: Spec -> String
prettyPrint = render . ppSpec
