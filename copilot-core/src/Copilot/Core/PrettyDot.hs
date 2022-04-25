-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | A pretty printer for Copilot specifications as GraphViz/dot graphs.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe  #-}

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

-- | Create a temporary/internal name from an extern variable name.
mkExtTmpVar :: String -> String
mkExtTmpVar = ("ext_" ++)

-- | Pretty print a Copilot Expression as a GraphViz graph part.
--
-- See the
-- <https://github.com/Copilot-Language/copilot-discussion/tree/master/TutorialAndDevGuide/DevGuide development guide> for details.
ppExprDot :: Int       -- ^ Index or ID of the next node in the graph.
          -> Int       -- ^ Index or ID of the parent node in the graph.
          -> Bool      -- ^ Mark externs with the prefix @externV:@.
          -> Expr a    -- ^ The expression to pretty print.
          -> (Doc,Int)
ppExprDot ii pere bb e0 = case e0 of
  Const t x                  -> (text (printf "%s [label=\"const: %s\",color=red1, style=filled]\n" (show ii::String) ((showWithType Haskell t x)::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)
  Drop _ 0 id                -> (text (printf "%s [label=\"stream: %s\",color=crimson, style=filled]\n" (show ii::String) (show id::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)
  Drop _ i id                ->  (text (printf "%s [label=\"drop %s: \nstream: %s\",color=crimson, style=filled]\n" (show ii::String) (show i::String) (show id::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)
  ExternVar _ name _         -> (if bb
                                   then (text (printf "%s [label=\"externV: %s\",color=cyan1, style=filled]\n" (show ii::String) (name::String)) <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)))
                                   else (text (printf "%s [label=\"%s\",color=cyan1, style=filled]\n" (show ii::String) (mkExtTmpVar name))      <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)))
                  ,ii+1)
  Local _ _ name e1 e2       -> let (r1, i1) = ppExprDot (ii+2) (ii+1) bb e1
                                in let (r2, i2) = ppExprDot (i1) ii bb e2
                                in (text (printf "%s [label=\"local:\",color=blue, style=filled]\n" (show ii::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> text (printf "%s [label=\"def: %s\",color=blue, style=filled]\n" ((show $ ii+1)::String) (name::String) )
                  <> text (printf "%s -> %s\n" (show ii::String) (show $ ii+1::String))
                  <> r1
                  <> r2, i2)

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
                  <> r2, i2)
  Op3 op e1 e2 e3            -> let (r1,i1) = ppExprDot (ii+1) ii bb e1
                                in let (r2,i2) = ppExprDot i1 ii bb e2
                                in let (r3,i3) = ppExprDot i2 ii bb e3
           in (text (printf "%s [label=\"op3: %s\",color=green4, style=filled]\n" (show ii::String) (ppOp3 op::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> r1
                  <> r2
                  <> r3, i3)

  Label _ s e                -> let (r1,i1) = ppExprDot (ii+1) ii bb e
           in (text (printf "%s [label=\"label: %s\",color=plum, style=filled]\n" (show ii::String) (s::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> r1,i1)

-- | Pretty print an untyped Copilot Expression as a graphiViz graph part.
--
-- See the
-- <https://github.com/Copilot-Language/copilot-discussion/tree/master/TutorialAndDevGuide/DevGuide development guide> for details.
ppUExpr :: Int        -- ^ Index or ID of the next node in the graph.
        -> Int        -- ^ Index or ID of the parent node in the graph.
        -> Bool       -- ^ Mark externs with the prefix @externV:@.
        -> UExpr      -- ^ The expression to pretty print.
        -> (Doc, Int)
ppUExpr i pere bb UExpr { uExprExpr = e0 } = ppExprDot i pere bb e0

-- | Pretty print a unary operator.
ppOp1 :: Op1 a b -> String
ppOp1 op = case op of
  Not       -> "not"
  Abs _     -> "abs"
  Sign _    -> "signum"
  Recip _   -> "recip"
  Exp _     -> "exp"
  Sqrt _    -> "sqrt"
  Log _     -> "log"
  Sin _     -> "sin"
  Tan _     -> "tan"
  Cos _     -> "cos"
  Asin _    -> "asin"
  Atan _    -> "atan"
  Acos _    -> "acos"
  Sinh _    -> "sinh"
  Tanh _    -> "tanh"
  Cosh _    -> "cosh"
  Asinh _   -> "asinh"
  Atanh _   -> "atanh"
  Acosh _   -> "acosh"
  Ceiling _ -> "ceiling"
  Floor _   -> "floor"
  BwNot _   -> "~"
  Cast _ _  -> "(cast)"

-- | Pretty print a binary operator.
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
  Atan2    _   -> "atan2"
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

-- | Pretty print a ternary operator.
ppOp3 :: Op3 a b c d -> String
ppOp3 op = case op of
  Mux _    -> "mux"

-- | Pretty print a stream as a GraphViz graph part.
ppStream :: Int        -- ^ Index or ID of the next node in the graph.
         -> Stream     -- ^ Stream to pretty print
         -> (Doc, Int)
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

-- | Pretty print a trigger as a GraphViz graph part.
ppTrigger :: Int        -- ^ Index or ID of the next node in the graph.
          -> Trigger    -- ^ Trigger to pretty print
          -> (Doc, Int)
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

-- | Pretty print a list of untyped Copilot Expressions as a GraphViz graph
-- part.
ppUExprL :: Int           -- ^ Index or ID of the next node in the graph.
         -> Int           -- ^ Index or ID of the parent node in the graph.
         -> Bool          -- ^ Mark externs with the prefix @externV:@.
         -> [UExpr]       -- ^ The list of expressions to pretty print.
         -> ([Doc], Int)
ppUExprL i _ _ [] = ([], i)

ppUExprL i pere bb (a:b) = ((r1:r2), i2)
  where
    (r1, i1) = ppUExpr i pere bb a
    (r2, i2) = ppUExprL i1 pere bb b

-- | Pretty print an observer as a GraphViz graph part.
ppObserver :: Int         -- ^ Index or ID of the next node in the graph.
           -> Observer    -- ^ Observer to pretty print
           -> (Doc, Int)
ppObserver i
  Observer
    { observerName     = name
    , observerExpr     = e }
  =
  (text (printf "%s [label=\"observer: \n%s\",color=mediumblue, style=filled]\n" (show i::String) name::String)
  <> r1, i1)
  where (r1, i1) = ppExprDot (i+1) i True e

-- | Pretty print a property as a GraphViz graph part.
ppProperty :: Int         -- ^ Index or ID of the next node in the graph.
           -> Property    -- ^ Property to pretty print
           -> (Doc, Int)
ppProperty i
  Property
    { propertyName     = name
    , propertyExpr     = e }
  =
  (text (printf "%s [label=\"property: \n%s\",color=mediumblue, style=filled]\n" (show i::String) name::String)
  <> r1, i1)
  where (r1, i1) = ppExprDot (i+1) i True e

-- | Pretty print a list of streams as a GraphViz graph part.
ppStreamL :: Int        -- ^ Index or ID of the next node in the graph.
          -> [Stream]   -- ^ List of streams to pretty print
          -> (Doc, Int)
ppStreamL i [] = (empty,i)

ppStreamL i (a:b) = ((s1$$s2),(i2))
  where
    (s1,i1) = ppStream i a
    (s2,i2) = ppStreamL i1 b

-- | Pretty print a list of triggers as a GraphViz graph part.
ppTriggerL :: Int        -- ^ Index or ID of the next node in the graph.
           -> [Trigger]  -- ^ List of triggers to pretty print
           -> (Doc, Int)
ppTriggerL i [] = (empty,i)

ppTriggerL i (a:b) = ((s1$$s2),(i2))
  where
    (s1,i1) = ppTrigger i a
    (s2,i2) = ppTriggerL i1 b

-- | Pretty print a list of observers as a GraphViz graph part.
ppObserverL :: Int         -- ^ Index or ID of the next node in the graph.
            -> [Observer]  -- ^ List of observers to pretty print
            -> (Doc, Int)
ppObserverL i [] = (empty,i)

ppObserverL i (a:b) = ((s1$$s2),(i2))
  where
    (s1,i1) = ppObserver i a
    (s2,i2) = ppObserverL i1 b

-- | Pretty print a list of properties as a GraphViz graph part.
ppPropertyL :: Int         -- ^ Index or ID of the next node in the graph.
            -> [Property]  -- ^ List of properties to pretty print
            -> (Doc, Int)
ppPropertyL i [] = (empty,i)

ppPropertyL i (a:b) = ((s1$$s2),(i2))
  where
    (s1,i1) = ppProperty i a
    (s2,i2) = ppPropertyL i1 b

-- | Pretty-print a Copilot specification as a GraphViz/dot graph.
ppSpecDot :: Int        -- ^ Index or ID of the next node in the graph.
          -> Spec       -- ^ Spec to pretty print.
          -> (Doc, Int)
ppSpecDot i spec =
  ((aa $$ cs $$ ds $$ es $$ fs $$ bb),i4)
  where
    aa = text "digraph G {\nnode [shape=box]\n"
    (cs, i1) = ppStreamL i    (specStreams    spec)
    (ds, i2) = ppTriggerL i1  (specTriggers   spec)
    (es, i3) = ppObserverL i2 (specObservers  spec)
    (fs, i4) = ppPropertyL i3 (specProperties spec)
    bb = text "\n}\n"

-- | Pretty-print a Copilot expression as a GraphViz/dot graph.
prettyPrintExprDot :: Bool     -- ^ Mark externs with the prefix @externV:@.
                   -> Expr a   -- ^ The expression to pretty print.
                   -> String
prettyPrintExprDot bb s = render rr
  where
    (r1, _) = ppExprDot 1 0 bb s
    rr = text "digraph G {\nnode [shape=box]\n" $$ (text "0 [label=\"file: \n?????\",color=red, style=filled]\n") <> r1 $$ text "\n}\n"

-- | Pretty-print a Copilot specification as a GraphViz/dot graph.
prettyPrintDot :: Spec -> String
prettyPrintDot s = render r1
  where (r1, _) = ppSpecDot 0 s
