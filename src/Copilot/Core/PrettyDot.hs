--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | A pretty printer for Copilot specifications.

{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}

module Copilot.Core.PrettyDot
  ( prettyPrintDot
  , ppExprDot
  ) where

import Copilot.Core
import Copilot.Core.Type.Show (showWithType, ShowType(..), showType)
import Prelude hiding (id)
import Text.PrettyPrint.HughesPJ
import Data.List (intersperse)
import Text.Printf

--------------------------------------------------------------------------------

strmName :: Int -> Doc
strmName id = text "s" <> int id

--------------------------------------------------------------------------------

ppExprDot :: Int -> Int -> Expr a -> (Doc,Int)
ppExprDot ii pere e0 = case e0 of
  Const t x                  -> (text (printf "%s [label=\"const: %s\",color=blue, style=filled]\n" (show ii::String) ((showWithType Haskell t x)::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)
  Drop _ 0 id                -> (text (printf "%s [label=\"stream: %s\",color=blue, style=filled]\n" (show ii::String) (show id::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)
  Drop _ i id                ->  (text (printf "%s [label=\"drop %s: %s\",color=blue, style=filled]\n" (show ii::String) (show i::String) (show id::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)
  ExternVar _ name _         -> (text (printf "%s [label=\"externV: %s\",color=cyan, style=filled]\n" (show ii::String) (name::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)
  ExternFun _ name args _ _  -> let (r1, i1) = ppUExprL (ii+1) ii args in
            (text (printf "%s [label=\"externF: %s\",color=cyan, style=filled]\n" (show ii::String) (name::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> (hcat (r1)),i1)
  ExternArray _ _ name 
              _ idx _ _      -> let (r1,i1) = ppExprDot (ii+1) ii idx 
           in (text (printf "%s [label=\"externA: %s\",color=cyan, style=filled]\n" (show ii::String) (name::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> r1,i1)

  ExternStruct _ name args _ -> let (r1, i1) = ppUExprL (ii+1) ii args in
            (text (printf "%s [label=\"externS: %s\",color=cyan, style=filled]\n" (show ii::String) (name::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> (hcat (r1)),i1)

  GetField _ _ name          -> (text (printf "%s [label=\"field: %s\",color=blue, style=filled]\n" (show ii::String) (name::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)

  Local _ _ name e1 e2       -> let (r1, i1) = ppExprDot (ii+2) (ii+1) e1 
                                in let (r2, i2) = ppExprDot (i1) ii e2
                                in (text (printf "%s [label=\"local:\",color=blue, style=filled]\n" (show ii::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> text (printf "%s [label=\"def: %s\",color=blue, style=filled]\n" ((show $ ii+1)::String) (name::String) )
                  <> text (printf "%s -> %s\n" (show ii::String) (show $ ii+1::String))
                  <> r1
                  <> r2 ,i2)

  Var _ name                 -> (text (printf "%s [label=\"var: %s\",color=blue, style=filled]\n" (show ii::String) (name::String) )
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String)),ii+1)

  Op1 op e                   -> let (r1,i1) = ppExprDot (ii+1) ii e 
           in (text (printf "%s [label=\"op1: %s\",color=green4, style=filled]\n" (show ii::String) (ppOp1 op::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> r1,i1)

  Op2 op e1 e2               -> let (r1,i1) = ppExprDot (ii+1) ii e1 
                                in let (r2,i2) = ppExprDot i1 ii e2 
           in (text (printf "%s [label=\"op2: %s\",color=green4, style=filled]\n" (show ii::String) (ppOp2 op::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> r1
                  <> r2 ,i2)
  Op3 op e1 e2 e3            -> let (r1,i1) = ppExprDot (ii+1) ii e1 
                                in let (r2,i2) = ppExprDot i1 ii e2 
                                in let (r3,i3) = ppExprDot i2 ii e3 
           in (text (printf "%s [label=\"op3: %s\",color=green4, style=filled]\n" (show ii::String) (ppOp3 op::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> r1
                  <> r2 
                  <> r3 ,i3)

  Label t s e                -> let (r1,i1) = ppExprDot (ii+1) ii e 
           in (text (printf "%s [label=\"label: %s\",color=green4, style=filled]\n" (show ii::String) (s::String))
                  <> text (printf "%s -> %s\n" (show pere::String) (show ii::String))
                  <> r1,i1)

ppUExpr :: Int -> Int -> UExpr -> (Doc, Int)
ppUExpr i pere UExpr { uExprExpr = e0 } = ppExprDot i pere e0

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
  --GetField _ _ -> "."

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
  (text (printf "%s [label=\"stream: %s\ntype: %s\",color=red, style=filled]\n" (show i::String) (show id::String) (showType t::String))
    <> text (printf "%s [label=\"++\",color=green, style=filled]\n" ((show $ i+1)::String))
    <> text (printf "%s -> %s\n" (show i::String) ((show $ i+1)::String))
    <> text (printf "%s [label=\"[%s]\",color=green, style=filled]\n" ((show $ i+2)::String) ((concat $ intersperse "," $ map (showWithType Haskell t) buffer )) ::String)
    <> text (printf "%s -> %s\n" (show i::String) ((show $ i+2)::String))
    <> r1, i1)
    where (r1, i1) = ppExprDot (i+3) i e
--------------------------------------------------------------------------------

ppTrigger :: Int -> Trigger -> (Doc, Int)
ppTrigger i
  Trigger
    { triggerName  = name
    , triggerGuard = e
    , triggerArgs  = args }
  =  ( text (printf "%s [label=\"trigger: %s\",color=red, style=filled]\n" (show i::String) (name::String) )
  <> text (printf "%s [label=\"guard\",color=yellow, style=filled]\n" ((show $ i+1)::String))
  <> text (printf "%s -> %s\n" (show i::String) ((show $ i+1)::String))
  <> r1
  <> text (printf "%s [label=\"args\",color=yellow, style=filled]\n" (show i1::String))
  <> text (printf "%s -> %s\n" (show i::String) (show i1::String))
  <>  (vcat (r2))
  ,i2)
  where 
    (r1, i1) = ppExprDot (i+2) (i+1) e
    (r2, i2) = ppUExprL (i1+1) (i1) args

ppUExprL :: Int -> Int -> [UExpr] -> ([Doc], Int)
ppUExprL i pere [] = ([], i)

ppUExprL i pere (a:b) = ((r1:r2), i2)
  where
    (r1, i1) = ppUExpr i pere a
    (r2, i2) = ppUExprL i1 pere b

--------------------------------------------------------------------------------

ppObserver :: Int -> Observer -> (Doc, Int)
ppObserver i
  Observer
    { observerName     = name
    , observerExpr     = e }
  =     
  (text (printf "%s [label=\"observer: \n%s\",color=red, style=filled]\n" (show i::String) name::String)
  <> r1, i1)
  where (r1, i1) = ppExprDot (i+1) i e
--------------------------------------------------------------------------------

ppProperty :: Int -> Property -> (Doc, Int)
ppProperty i
  Property
    { propertyName     = name
    , propertyExpr     = e }
  =   
  (text (printf "%s [label=\"property: \n%s\",color=red, style=filled]\n" (show i::String) name::String)
  <> r1, i1)
  where (r1, i1) = ppExprDot (i+1) i e


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

-- | Pretty-prints a Copilot specification.
prettyPrintDot :: Spec -> String
prettyPrintDot s = render r1
  where (r1,i1) = (ppSpecDot 0 s)

--------------------------------------------------------------------------------
