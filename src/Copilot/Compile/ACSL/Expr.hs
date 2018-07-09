{-# LANGUAGE GADTs #-}

module Copilot.Compile.ACSL.Expr where

import Copilot.Core ( Op1 (..)
                    , Op2 (..)
                    , Op3 (..)
                    , Stream (..)
                    , Expr (..)
                    , Id
                    )
import Copilot.Core.Type.Show ( showWithType
                              , ShowType (..)
                              )
import Copilot.Compile.C.Meta

import Data.List  ( find )
import Data.Maybe ( fromJust )
import Text.PrettyPrint

{- Translate Core.Expr to ACSL -}
exprACSL :: AProgram -> Expr a -> Doc
exprACSL ap expr = let  gens = generators ap
                        exts = externals ap
                   in case expr of
  Const ty x -> text $ showWithType Haskell ty x

  Drop _ n id ->  let gen = lookupGenerator id gens
                      buff = genBuffName gen
                      idx = genIndexName gen
                      len (Stream _ b _ _) = length b
                  in buffidx buff idx (fromIntegral n) (len $ genStream gen)

  Local _ _ n e1 e2 -> text "\\let" <+> text n <+> equals <+> exprACSL ap e1
                                    <+> semi <+> exprACSL ap e2

  Var _ n -> text n

  ExternVar _ n _ ->  let ex = lookupExternal n exts
                      in text $ exLocName ex

  -- ExternFun

  Op1 op e -> op1ACSL op (exprACSL ap e)

  Op2 op e1 e2 -> op2ACSL op (exprACSL ap e1) (exprACSL ap e2)

  Op3 op e1 e2 e3 -> op3ACSL op (exprACSL ap e1)
                                (exprACSL ap e2)
                                (exprACSL ap e3)

  -- Label


{- Returns code that take an index of a buffer, possibly with a drop n -}
buffidx :: String -> String -> Int -> Int -> Doc
buffidx buff idx n len = text buff <> brackets idx' where
  idx' | n == 0    = text idx
       | otherwise = parens (text idx <> char '+' <> int n) <> char '%' <> int len

{- Some helper functions -}
lookupExternal :: String -> [External] -> External
lookupExternal n exts = fromJust $ find (\(External n' _ _) -> n == n') exts

lookupGenerator :: Id -> [Generator] -> Generator
lookupGenerator id gens = fromJust $ find match gens where
  match = (\(Generator _ _ _ _ (Stream id' _ _ _)) -> id == id')



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
