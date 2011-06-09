--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | A pretty printer for Copilot specifications.

module Copilot.Core.PrettyPrint
  ( prettyPrint
  ) where

import Copilot.Core
import Copilot.Core.Type.Show (showWithType)
import Prelude hiding (id)
import Text.PrettyPrint.HughesPJ

--------------------------------------------------------------------------------

newtype PPExpr a         = PPExpr { ppExpr :: Doc }
newtype PPOp1  a b       = PPOp1  { ppOp1  :: Doc -> Doc }
newtype PPOp2  a b c     = PPOp2  { ppOp2  :: Doc -> Doc -> Doc }
newtype PPOp3  a b c d   = PPOp3  { ppOp3  :: Doc -> Doc -> Doc -> Doc }

instance Expr PPExpr where
  const t x          = PPExpr $ text (showWithType t x)
  drop _ 0 id        = PPExpr $ text "stream" <+> text "s" <> int id
  drop _ i id        = PPExpr $ text "drop" <+> text (show i) <+> text "s" <> int id
  extern _ name      = PPExpr $ text "extern \"" <> text name <> text "\""
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
      = text "stream: \"s" <>  int id <> text "\""
    <+> text (show $ map (showWithType t) buffer)
    <+> text "="
    <+> ppExpr e

--------------------------------------------------------------------------------

ppTrigger :: Trigger -> Doc
ppTrigger
  Trigger
    { triggerName  = name
    , triggerGuard = e
    , triggerArgs  = args }
  =   text "trigger: \"" <> text name <> text "\""
  <+> text "="
  <+> ppExpr e
  $$  nest 2 (foldr (($$) . ppTriggerArg) empty argsAndNum)

  where

  argsAndNum :: [(TriggerArg, Int)]
  argsAndNum = zip args [0..]

  ppTriggerArg :: (TriggerArg, Int) -> Doc
  ppTriggerArg (TriggerArg e1 _, k)
    =   text "arg: " <> int k
    <+> text "="
    <+> ppExpr e1

--------------------------------------------------------------------------------

ppSpec :: Spec -> Doc
ppSpec spec = cs $$ ds
  where
    cs = foldr (($$) . ppStream)  empty (specStreams spec)
    ds = foldr (($$) . ppTrigger) empty (specTriggers spec)

--------------------------------------------------------------------------------

-- | Pretty-prints a Copilot specification.
prettyPrint :: Spec -> String
prettyPrint = render . ppSpec

--------------------------------------------------------------------------------