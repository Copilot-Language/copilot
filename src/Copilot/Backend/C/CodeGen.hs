{-# LANGUAGE GADTs #-}

module Copilot.Backend.C.CodeGen
  ( compile
  ) where

import Copilot.Core as CP
import Copilot.Backend.C.Tmp

import Language.C99.AST as C
import Language.C99.Util

recip_def :: FunDef
recip_def = FD float name args body where
  name = Dr Nothing (DDIdent $ ident "recip")
  args = Just $ funarg float "e"
  body = CS [ BIStmt $ SJump $ JSReturn $ Just $ EDiv (constint 1) (var "e") ]

funarg ty name = DnLBase (Dn ty (Just $ IDLBase $ IDDeclr $ (Dr Nothing (DDIdent $ ident name))))

ty2type :: Type a -> DeclnSpecs
ty2type ty = case ty of
  Int8    -> typedefty "int8_t"
  Int16   -> typedefty "int16_t"
  Int32   -> typedefty "int32_t"
  Int64   -> typedefty "int64_t"
  Word8   -> typedefty "uint8_t"
  Word16  -> typedefty "uint16_t"
  Word32  -> typedefty "uint32_t"
  Word64  -> typedefty "uint64_t"
  Float   -> float
  Double  -> double
  Bool    -> typedefty "bool"

op1 :: Op1 a b -> C.Expr -> C.Expr
op1 op e = case op of
  Not     -> EUn UNot e
  Abs _   -> funcall "abs" [e]
  Sign _  -> funcall "sign" [e] -- TODO implement function
  Recip _ -> funcall "recip" [e]
  Exp _   -> funcall "exp" [e]
  Sqrt _  -> funcall "sqrt" [e]
  Log _   -> funcall "log" [e]
  Sin _   -> funcall "sin" [e]
  Cos _   -> funcall "cos" [e]
  Asin _  -> funcall "asin" [e]
  Atan _  -> funcall "atan" [e]
  Acos _  -> funcall "acos" [e]
  Sinh _  -> funcall "sinh" [e]
  Tanh _  -> funcall "tanh" [e]
  Cosh _  -> funcall "cosh" [e]
  Asinh _ -> funcall "asinh" [e]
  Atanh _ -> funcall "atanh" [e]
  Acosh _ -> funcall "acosh" [e]
  BwNot _ -> EUn UBNot e
  Cast ty _ -> undefined -- TODO

op2 :: Op2 a b c -> C.Expr -> C.Expr -> C.Expr
op2 op = case op of
  And     -> EAnd
  Or      -> EOr
  Add _  -> EAdd
  Sub _  -> ESub
  Mul _  -> EMult
  Mod _  -> EMod
  Div _  -> EDiv
  Fdiv _ -> EDiv
  Pow _  -> \b n -> funcall "pow" [b, n]
  Logb _ -> undefined -- TODO
  Eq _   -> EEq
  Ne _   -> ENEq
  Le _   -> ELE
  Ge  _  -> EGE
  Lt   _ -> ELT
  Gt _   -> EGT
  BwAnd _   -> ELAnd
  BwOr _    -> ELOr
  BwXor _   -> EXor
  BwShiftL _ _ -> EShiftL
  BwShiftR _ _ -> EShiftR

op3 :: Op3 a b c d -> C.Expr -> C.Expr -> C.Expr -> C.Expr
op3 op = case op of
  Mux _   -> ECond

compile = undefined
