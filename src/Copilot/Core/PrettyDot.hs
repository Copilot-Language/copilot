--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | A pretty printer for Copilot specifications.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}

module Copilot.Core.PrettyDot
  ( prettyPrintDot
  , prettyPrintExprDot
  ) where

import Copilot.Core
import Copilot.Core.Type.Show (showWithType, ShowType(..), showType)
import Prelude hiding (id, (<>))
import Text.PrettyPrint.HughesPJ
import Data.List (intersperse)
import Text.Printf

mkExtTmpVar :: String -> String
mkExtTmpVar = ("ext_" ++)

mkExtTmpTag :: String -> Maybe Tag -> String
mkExtTmpTag name tag = "ext_" ++ name ++ "_" ++ show (tagExtract tag)

tagExtract :: Maybe Tag -> Tag
tagExtract Nothing = impossible "tagExtract" "copilot-sbv"
tagExtract (Just tag) = tag

--------------------------------------------------------------------------------

ppExprDot :: Int -> Int -> Bool -> Expr a -> (Doc,Int)
ppExprDot ii pere bb e0 = case e0 of
  Const t x                  -> (text (printf "%s [label=\"const: %s\",color=red1, style=filled]\n" (show ii::String) ((showWithType Haskell t x)::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)
  Drop _ 0 id                -> (text (printf "%s [label=\"stream: %s\",color=crimson, style=filled]\n" (show ii::String) (show id::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)
  Drop _ i id                ->  (text (printf "%s [label=\"drop %s: \nstream: %s\",color=crimson, style=filled]\n" (show ii::String) (show i::String) (show id::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)
  ExternVar _ name _         -> (if bb then (text (printf "%s [label=\"externV: %s\",color=cyan1, style=filled]\n" (show ii::String) (name::String)) <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))) else (text (printf "%s [label=\"%s\",color=cyan1, style=filled]\n" (show ii::String) (mkExtTmpVar name)) <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)))
                  ,ii+1)
  Local _ _ name e1 e2       -> let (r1, i1) = ppExprDot (ii+2) (ii+1) bb e1
                                in let (r2, i2) = ppExprDot (i1) ii bb e2
                                in (text (printf "%s [label=\"local:\",color=blue, style=filled]\n" (show ii::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> text (printf "%s [label=\"def: %s\",color=blue, style=filled]\n" ((show $ ii+1)::String) (name::String) )
                  <> text (printf "%s -> %s\n" (show ii::String) (show $ ii+1::String))
                  <> r1
                  <> r2 ,i2)

  Var _ name                 -> (text (printf "%s [label=\"var: %s\",color=blue, style=filled]\n" (show ii::String) (name::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)

  Op1 op e                   -> let (r1,i1) = ppExprDot (ii+1) ii bb e
           in (text (printf "%s [label=\"op1: %s\",color=green4, style=filled]\n" (show ii::String) (ppOp1 op::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> r1,i1)

  Op2 op e1 e2               -> let (r1,i1) = ppExprDot (ii+1) ii bb e1
                                in let (r2,i2) = ppExprDot i1 ii bb e2
           in (text (printf "%s [label=\"op2: %s\",color=green4, style=filled]\n" (show ii::String) (ppOp2 op::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> r1
                  <> r2 ,i2)
  Op3 op e1 e2 e3            -> let (r1,i1) = ppExprDot (ii+1) ii bb e1
                                in let (r2,i2) = ppExprDot i1 ii bb e2
                                in let (r3,i3) = ppExprDot i2 ii bb e3
           in (text (printf "%s [label=\"op3: %s\",color=green4, style=filled]\n" (show ii::String) (ppOp3 op::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> r1
                  <> r2
                  <> r3 ,i3)

  Label _ s e                -> let (r1,i1) = ppExprDot (ii+1) ii bb e
           in (text (printf "%s [label=\"label: %s\",color=plum, style=filled]\n" (show ii::String) (s::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> r1,i1)

ppUExpr :: Int -> Int -> Bool -> UExpr -> (Doc, Int)
ppUExpr i pere bb UExpr { uExprExpr = e0 } = ppExprDot i pere bb e0

ppOp1 :: Op1 a b -> String
ppOp1 op = case op of
  Not      -> "not"
  Abs _    -> "abs"
  Sign _   -> "signum"
  Recip _  -> "recip"
  Exp _    -> "exp"
  Sqrt _   -> "sqrt"
  Log _    -> "log"
  Sin _    -> "sin"
  Tan _    -> "tan"
  Cos _    -> "cos"
  Asin _   -> "asin"
  Atan _   -> "atan"
  Acos _   -> "acos"
  Sinh _   -> "sinh"
  Tanh _   -> "tanh"
  Cosh _   -> "cosh"
  Asinh _  -> "asinh"
  Atanh _  -> "atanh"
  Acosh _  -> "acosh"
  BwNot _  -> "~"
  Cast _ _ -> "(cast)"

ppOp2 :: Op2 a b c -> String
ppOp2 op = case op of
  And          -> "&&"
  Or           -> "||"
  Add      _   -> "+"
  Sub      _   -> "-"
  Mul      _   -> "*"
  Div      _   -> "div"
  Mod      _   -> "mod"
  Fdiv     _   -> "/"
  Pow      _   -> "**"
  Logb     _   -> "logBase"
  Eq       _   -> "=="
  Ne       _   -> "/="
  Le       _   -> "<="
  Ge       _   -> ">="
  Lt       _   -> "<"
  Gt       _   -> ">"
  BwAnd    _   -> "&"
  BwOr     _   -> "|"
  BwXor    _   -> "^"
  BwShiftL _ _ -> "<<"
  BwShiftR _ _ -> ">>"

ppOp3 :: Op3 a b c d -> String
ppOp3 op = case op of
  Mux _    -> "mux"

--------------------------------------------------------------------------------

ppStream :: Int -> Stream -> (Doc, Int)
ppStream i
  Stream
    { streamId       = id
    , streamBuffer   = buffer
    , streamExpr     = e
    , streamExprType = t
    }
      =
  (text (printf "%s [label=\"stream: %s\ntype: %s\",color=mediumblue, style=filled]\n" (show i::String) (show id::String) (showType t::String))
    <> text (printf "%s [label=\"++\",color=yellow, style=filled]\n" ((show $ i+1)::String))
    <> text (printf "%s -> %s\n" (show i::String) ((show $ i+1)::String))
    <> text (printf "%s [label=\"[%s]\",color=green, style=filled]\n" ((show $ i+2)::String) ((concat $ intersperse "," $ map (showWithType Haskell t) buffer )) ::String)
    <> text (printf "%s -> %s\n" (show (i+1)::String) ((show $ i+2)::String))
    <> r1, i1)
    where (r1, i1) = ppExprDot (i+3) (i+1) True e
--------------------------------------------------------------------------------

ppTrigger :: Int -> Trigger -> (Doc, Int)
ppTrigger i
  Trigger
    { triggerName  = name
    , triggerGuard = e
    , triggerArgs  = args }
  =  ( text (printf "%s [label=\"trigger: %s\",color=mediumblue, style=filled]\n" (show i::String) (name::String) )
  <> text (printf "%s [label=\"guard\",color=yellow, style=filled]\n" ((show $ i+1)::String))
  <> text (printf "%s -> %s\n" (show i::String) ((show $ i+1)::String))
  <> r1
  <> text (printf "%s [label=\"args\",color=yellow, style=filled]\n" (show i1::String))
  <> text (printf "%s -> %s\n" (show i::String) (show i1::String))
  <>  (vcat (r2))
  ,i2)
  where
    (r1, i1) = ppExprDot (i+2) (i+1) True e
    (r2, i2) = ppUExprL (i1+1) (i1) True args

ppUExprL :: Int -> Int -> Bool -> [UExpr] -> ([Doc], Int)
ppUExprL i _ _ [] = ([], i)

ppUExprL i pere bb (a:b) = ((r1:r2), i2)
  where
    (r1, i1) = ppUExpr i pere bb a
    (r2, i2) = ppUExprL i1 pere bb b

--------------------------------------------------------------------------------

ppObserver :: Int -> Observer -> (Doc, Int)
ppObserver i
  Observer
    { observerName     = name
    , observerExpr     = e }
  =
  (text (printf "%s [label=\"observer: \n%s\",color=mediumblue, style=filled]\n" (show i::String) name::String)
  <> r1, i1)
  where (r1, i1) = ppExprDot (i+1) i True e
--------------------------------------------------------------------------------

ppProperty :: Int -> Property -> (Doc, Int)
ppProperty i
  Property
    { propertyName     = name
    , propertyExpr     = e }
  =
  (text (printf "%s [label=\"property: \n%s\",color=mediumblue, style=filled]\n" (show i::String) name::String)
  <> r1, i1)
  where (r1, i1) = ppExprDot (i+1) i True e

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

ppStreamL :: Int -> [Stream] -> (Doc, Int)
ppStreamL i [] = (empty,i)

ppStreamL i (a:b) = ((s1$$s2),(i2))
  where
    (s1,i1) = ppStream i a
    (s2,i2) = ppStreamL i1 b

--------------------------------------------------------------------------------

ppTriggerL :: Int -> [Trigger] -> (Doc, Int)
ppTriggerL i [] = (empty,i)

ppTriggerL i (a:b) = ((s1$$s2),(i2))
  where
    (s1,i1) = ppTrigger i a
    (s2,i2) = ppTriggerL i1 b

--------------------------------------------------------------------------------

ppObserverL :: Int -> [Observer] -> (Doc, Int)
ppObserverL i [] = (empty,i)

ppObserverL i (a:b) = ((s1$$s2),(i2))
  where
    (s1,i1) = ppObserver i a
    (s2,i2) = ppObserverL i1 b


--------------------------------------------------------------------------------

ppPropertyL :: Int -> [Property] -> (Doc, Int)
ppPropertyL i [] = (empty,i)

ppPropertyL i (a:b) = ((s1$$s2),(i2))
  where
    (s1,i1) = ppProperty i a
    (s2,i2) = ppPropertyL i1 b

--------------------------------------------------------------------------------

ppSpecDot :: Int -> Spec -> (Doc, Int)
ppSpecDot i spec =
  ((aa $$ cs $$ ds $$ es $$ fs $$ bb),i4)
  where
    aa = text "digraph G {\nnode [shape=box]\n"
    (cs, i1) = ppStreamL i    (specStreams    spec)
    (ds, i2) = ppTriggerL i1  (specTriggers   spec)
    (es, i3) = ppObserverL i2 (specObservers  spec)
    (fs, i4) = ppPropertyL i3 (specProperties spec)
    bb = text "\n}\n"

--------------------------------------------------------------------------------

-- | Pretty-prints a Copilot expression.
prettyPrintExprDot :: Bool -> Expr a -> String
prettyPrintExprDot bb s = render rr
  where
    (r1, _) = ppExprDot 1 0 bb s
    rr = text "digraph G {\nnode [shape=box]\n" $$ (text "0 [label=\"file: \n?????\",color=red, style=filled]\n") <> r1 $$ text "\n}\n"

-- | Pretty-prints a Copilot specification.
prettyPrintDot :: Spec -> String
prettyPrintDot s = render r1
  where (r1, _) = ppSpecDot 0 s

--------------------------------------------------------------------------------
