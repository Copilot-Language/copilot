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
import Data.List (intersperse)

--------------------------------------------------------------------------------

newtype PPExpr a         = PPExpr { ppExpr :: Doc }
newtype PPOp1  a b       = PPOp1  { ppOp1  :: Doc -> Doc }
newtype PPOp2  a b c     = PPOp2  { ppOp2  :: Doc -> Doc -> Doc }
newtype PPOp3  a b c d   = PPOp3  { ppOp3  :: Doc -> Doc -> Doc -> Doc }

instance Expr PPExpr where
  const t x            = PPExpr $ text (showWithType t x)
  drop _ 0 id          = PPExpr $ text "stream" <+> text "s" <> int id
  drop _ i id          = PPExpr $ text "drop" <+> text (show i) <+> text "s" <>
                         int id
  extern _ name        = PPExpr $ text "extern \"" <> text name <> text "\""
  local _ _ name e1 e2 = PPExpr $ text "local \"" <> text name <> text "\" ="
                         <+> ppExpr e1 <+> text "in" <+> ppExpr e2
  var _ name           = PPExpr $ text "var \"" <> text name <> text "\""
  op1 op e             = PPExpr $ ppOp1 op (ppExpr e)
  op2 op e1 e2         = PPExpr $ ppOp2 op (ppExpr e1) (ppExpr e2)
  op3 op e1 e2 e3      = PPExpr $ ppOp3 op (ppExpr e1) (ppExpr e2) (ppExpr e3)

instance Op1 PPOp1 where
  not      = PPOp1 $ ppPrefix "not"
  abs _    = PPOp1 $ ppPrefix "abs"
  sign _   = PPOp1 $ ppPrefix "signum"
  recip _  = PPOp1 $ ppPrefix "recip"
  exp _    = PPOp1 $ ppPrefix "exp"
  sqrt _   = PPOp1 $ ppPrefix "sqrt"
  log _    = PPOp1 $ ppPrefix "log"
  sin _    = PPOp1 $ ppPrefix "sin"
  tan _    = PPOp1 $ ppPrefix "tan"
  cos _    = PPOp1 $ ppPrefix "cos"
  asin _   = PPOp1 $ ppPrefix "asin"
  atan _   = PPOp1 $ ppPrefix "atan"
  acos _   = PPOp1 $ ppPrefix "acos"
  sinh _   = PPOp1 $ ppPrefix "sinh"
  tanh _   = PPOp1 $ ppPrefix "tanh"
  cosh _   = PPOp1 $ ppPrefix "cosh"
  asinh _  = PPOp1 $ ppPrefix "asinh"
  atanh _  = PPOp1 $ ppPrefix "atanh"
  acosh _  = PPOp1 $ ppPrefix "acosh"

instance Op2 PPOp2 where
  and      = PPOp2 $ ppInfix "&&"
  or       = PPOp2 $ ppInfix "||"
  add _    = PPOp2 $ ppInfix "+"
  sub _    = PPOp2 $ ppInfix "-"
  mul _    = PPOp2 $ ppInfix "*"
  div _    = PPOp2 $ ppInfix "div"
  mod _    = PPOp2 $ ppInfix "mod"
  fdiv _   = PPOp2 $ ppInfix "/"
  pow _    = PPOp2 $ ppInfix "**"
  logb _   = PPOp2 $ ppInfix "logBase"
  eq _     = PPOp2 $ ppInfix "=="
  ne _     = PPOp2 $ ppInfix "/="
  le _     = PPOp2 $ ppInfix "<="
  ge _     = PPOp2 $ ppInfix ">="
  lt _     = PPOp2 $ ppInfix "<"
  gt _     = PPOp2 $ ppInfix ">"

instance Op3 PPOp3 where
  mux _    = PPOp3 $ \ doc1 doc2 doc3 ->
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
      = text "stream: \"s" <> int id <> text "\""
    <+> text "="
    <+> text ("["
              ++ ( concat $ intersperse "," $ map (showWithType t) buffer )
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

ppObserver :: Observer -> Doc
ppObserver
  Observer
    { observerName     = name
    , observerExpr     = e }
  =   text "observer: \"" <> text name <> text "\""
  <+> text "="
  <+> ppExpr e

--------------------------------------------------------------------------------

ppSpec :: Spec -> Doc
ppSpec spec = cs $$ ds $$ es
  where
    cs = foldr (($$) . ppStream)   empty (specStreams   spec)
    ds = foldr (($$) . ppTrigger)  empty (specTriggers  spec)
    es = foldr (($$) . ppObserver) empty (specObservers spec)

--------------------------------------------------------------------------------

-- | Pretty-prints a Copilot specification.
prettyPrint :: Spec -> String
prettyPrint = render . ppSpec

--------------------------------------------------------------------------------
