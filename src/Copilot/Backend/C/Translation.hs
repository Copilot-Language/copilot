{-# LANGUAGE GADTs #-}

{- File containing functions that translates simple expressions to C. -}

module Copilot.Backend.C.Translation where

import Copilot.Backend.C.Tmp

import Copilot.Core as CP
import Language.C99.AST as C hiding (Struct)
import Language.C99.Util

ty2type :: Type a -> DeclnSpecs
ty2type ty = case ty of
  Int8      -> typedefty "int8_t"
  Int16     -> typedefty "int16_t"
  Int32     -> typedefty "int32_t"
  Int64     -> typedefty "int64_t"
  Word8     -> typedefty "uint8_t"
  Word16    -> typedefty "uint16_t"
  Word32    -> typedefty "uint32_t"
  Word64    -> typedefty "uint64_t"
  Float     -> float
  Double    -> double
  Bool      -> typedefty "bool"
  Array tya -> ty2type tya
  Struct s  -> case typename s of
    TyTypedef n -> typedefty n
    TyStruct n  -> struct n

op1 :: Op1 a b -> C.Expr -> C.Expr
op1 op e = case op of
  Not     -> EUn UNot e
  Abs _   -> funcall "abs" [e]
  Sign _  -> funcall "sign" [e] -- TODO implement function
  Recip _ -> EDiv (constint 1) (var "e")
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
  And     -> ELAnd
  Or      -> ELOr
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
  BwAnd _   -> EAnd
  BwOr _    -> EOr
  BwXor _   -> EXor
  BwShiftL _ _ -> EShiftL
  BwShiftR _ _ -> EShiftR

op3 :: Op3 a b c d -> C.Expr -> C.Expr -> C.Expr -> C.Expr
op3 op = case op of
  Mux _   -> ECond

{- C code for a given constant and type -}
constty :: Type a -> a -> C.Expr
constty Int8    = constint
constty Int16   = constint
constty Int32   = constint
constty Int64   = constint
constty Word8   = constword
constty Word16  = constword
constty Word32  = constword
constty Word64  = constword
constty Bool    = constbool
constty Float   = constfloat
constty Double  = constdouble
