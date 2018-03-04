{-# LANGUAGE GADTs #-}

module Copilot.Compile.ACSL.Expr where

import Copilot.Core ( Op1 (..)
                    , Op2 (..)
                    , Op3 (..)
                    )

import Text.PrettyPrint


op1ACSL :: Op1 a b -> Doc -> Doc
op1ACSL op = case op of
  Not       -> prefixop "!"
  Abs   _   -> prefixop "\\abs"
  Sign  _   -> \x -> ifElse (x <> text " > 0")
                        (int 1)
                        (ifElse (x <> text " < 0")
                            (int (-1))
                            (int 0)
                        )
  Recip _   -> prefixop "\\recip"
  Exp   _   -> prefixop "\\exp"
  Sqrt  _   -> prefixop "\\sqrt"
  Log   _   -> prefixop "\\log"
  Sin   _   -> prefixop "\\sin"
  Tan   _   -> prefixop "\\tan"
  Cos   _   -> prefixop "\\cos"
  Asin  _   -> prefixop "\\asin"
  Atan  _   -> prefixop "\\atan"
  Acos  _   -> prefixop "\\acos"
  Sinh  _   -> prefixop "\\sinh"
  Tanh  _   -> prefixop "\\tanh"
  Cosh  _   -> prefixop "\\cosh"
  Asinh _   -> prefixop "\\asinh"
  Atanh _   -> prefixop "\\atanh"
  Acosh _   -> prefixop "\\acosh"
  BwNot _   -> prefixop "~"
  Cast  _ _ -> prefixop ""

op2ACSL :: Op2 a b c -> Doc -> Doc -> Doc
op2ACSL op = case op of
  And           -> infixop "&&"
  Or            -> infixop "||"
  Add       _   -> infixop "+"
  Sub       _   -> infixop "-"
  Mul       _   -> infixop "*"
  Mod       _   -> infixop "%"
  Div       _   -> infixop "/"
  Fdiv      _   -> infixop "/"
  Pow       _   -> prefixop2 "\\pow"
  Logb      _   -> \a b -> parens (text "\\log" <> a)
                           <> char '/' <>
                           parens (text "\\log" <> b)
  Eq        _   -> infixop "=="
  Ne        _   -> infixop "!="
  Le        _   -> infixop "<="
  Ge        _   -> infixop ">="
  Lt        _   -> infixop "<"
  Gt        _   -> infixop ">"
  BwAnd     _   -> infixop "&"
  BwOr      _   -> infixop "|"
  BwXor     _   -> infixop "^"
  BwShiftL  _ _ -> infixop "<<"
  BwShiftR  _ _ -> infixop ">>"

op3ACSL :: Op3 a b c d -> Doc -> Doc -> Doc -> Doc
op3ACSL op e1 e2 e3 = case op of
  Mux _ -> ifElse e1 e2 e3

prefixop :: String -> Doc -> Doc
prefixop op e = parens $ text op <+> e

prefixop2 :: String -> Doc -> Doc -> Doc
prefixop2 op e1 e2 = parens $ text op <+> e1 <> comma <+> e2

infixop :: String -> Doc -> Doc -> Doc
infixop op e1 e2 = parens $ e1 <+> text op <+> e2

ifElse :: Doc -> Doc -> Doc -> Doc
ifElse i t e = parens (i <> char '?' <> t <> colon <> e)
