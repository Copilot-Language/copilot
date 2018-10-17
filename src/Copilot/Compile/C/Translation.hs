{-# LANGUAGE GADTs #-}

{- File containing functions that translates simple expressions to C. -}

module Copilot.Compile.C.Translation where

import Copilot.Compile.C.Tmp hiding (var)

import Copilot.Core as CP
import Language.C99 as C hiding (var)
import Language.C99.Util       hiding (Expr, funcall)

ty2type :: Type a -> DeclnSpecs
ty2type ty = case ty of
  Int8      -> typedef' "int8_t"
  Int16     -> typedef' "int16_t"
  Int32     -> typedef' "int32_t"
  Int64     -> typedef' "int64_t"
  Word8     -> typedef' "uint8_t"
  Word16    -> typedef' "uint16_t"
  Word32    -> typedef' "uint32_t"
  Word64    -> typedef' "uint64_t"
  Float     -> float
  Double    -> double
  Bool      -> typedef' "bool"
  Array tya -> ty2type tya
  CP.Struct s  -> struct $ typename s

ty2typespec :: Type a -> TypeSpec
ty2typespec ty = let td name = TTypedef (TypedefName $ ident name) in case ty of
  Int8      -> td "int8_t"
  Int16     -> td "int16_t"
  Int32     -> td "int32_t"
  Int64     -> td "int64_t"
  Word8     -> td "uint8_t"
  Word16    -> td "uint16_t"
  Word32    -> td "uint32_t"
  Word64    -> td "uint64_t"
  Float     -> TFloat
  Double    -> TDouble
  Bool      -> td "bool"
  Array tya -> ty2typespec tya
  CP.Struct s  -> TStructOrUnion $ StructOrUnionForwDecln C.Struct (ident $ typename s)

op1 :: Op1 a b -> C.Expr -> C.Expr
op1 op e = case op of
  Not     -> wrap $ UnaryOp UONot (wrap e)
  Abs _   -> wrap $ funcall "abs" [wrap e]
  Sign _  -> wrap $ funcall "sign" [wrap e] -- TODO implement function
  Recip _ -> wrap $ MultDiv (wrap $ constint 1) (wrap $ eval $ var "e")
  Exp _   -> wrap $ funcall "exp" [wrap e]
  Sqrt _  -> wrap $ funcall "sqrt" [wrap e]
  Log _   -> wrap $ funcall "log" [wrap e]
  Sin _   -> wrap $ funcall "sin" [wrap e]
  Cos _   -> wrap $ funcall "cos" [wrap e]
  Asin _  -> wrap $ funcall "asin" [wrap e]
  Atan _  -> wrap $ funcall "atan" [wrap e]
  Acos _  -> wrap $ funcall "acos" [wrap e]
  Sinh _  -> wrap $ funcall "sinh" [wrap e]
  Tanh _  -> wrap $ funcall "tanh" [wrap e]
  Cosh _  -> wrap $ funcall "cosh" [wrap e]
  Asinh _ -> wrap $ funcall "asinh" [wrap e]
  Atanh _ -> wrap $ funcall "atanh" [wrap e]
  Acosh _ -> wrap $ funcall "acosh" [wrap e]
  BwNot _ -> wrap $ UnaryOp UOBNot (wrap e)
  CP.Cast ty _ -> undefined -- TODO
  GetField _ _ n -> wrap $ PostfixDot (wrap e) (ident n)

op2 :: Op2 a b c -> C.Expr -> C.Expr -> C.Expr
op2 op e1 e2 = case op of
  CP.And        -> wrap $ C.LAnd      (wrap e1) (wrap e2)
  CP.Or         -> wrap $ C.LOr       (wrap e1) (wrap e2)
  Add _         -> wrap $ AddPlus     (wrap e1) (wrap e2)
  Sub _         -> wrap $ AddMin      (wrap e1) (wrap e2)
  Mul _         -> wrap $ MultMult    (wrap e1) (wrap e2)
  Mod _         -> wrap $ MultMod     (wrap e1) (wrap e2)
  Div _         -> wrap $ MultDiv     (wrap e1) (wrap e2)
  Fdiv _        -> wrap $ MultDiv     (wrap e1) (wrap e2)
  Pow _         -> wrap $ (\b n -> funcall "pow" [b, n]) (wrap e1) (wrap e2)
  Logb _        -> undefined -- TODO
  Eq _          -> wrap $ EqEq        (wrap e1) (wrap e2)
  Ne _          -> wrap $ EqNEq       (wrap e1) (wrap e2)
  Le _          -> wrap $ RelLE       (wrap e1) (wrap e2)
  Ge  _         -> wrap $ RelGE       (wrap e1) (wrap e2)
  Lt   _        -> wrap $ RelLT       (wrap e1) (wrap e2)
  Gt _          -> wrap $ RelGT       (wrap e1) (wrap e2)
  BwAnd _       -> wrap $ C.And       (wrap e1) (wrap e2)
  BwOr _        -> wrap $ C.Or        (wrap e1) (wrap e2)
  BwXor _       -> wrap $ C.XOr       (wrap e1) (wrap e2)
  BwShiftL _ _  -> wrap $ ShiftLeft   (wrap e1) (wrap e2)
  BwShiftR _ _  -> wrap $ ShiftRight  (wrap e1) (wrap e2)

  Index _       -> wrap $ PostfixIndex (wrap e1) (wrap e2)

op3 :: Op3 a b c d -> C.Expr -> C.Expr -> C.Expr -> C.Expr
op3 op e1 e2 e3 = case op of
  Mux _   -> wrap $ Cond (wrap e1) (wrap e2) (wrap e3)

{- C code for a given constant and type -}
constty :: Type a -> a -> C.Expr
constty Int8    = wrap.constint.fromIntegral
constty Int16   = wrap.constint.fromIntegral
constty Int32   = wrap.constint.fromIntegral
constty Int64   = wrap.constint.fromIntegral
constty Word8   = wrap.constword.fromIntegral
constty Word16  = wrap.constword.fromIntegral
constty Word32  = wrap.constword.fromIntegral
constty Word64  = wrap.constword.fromIntegral
constty Bool    = wrap.constbool
constty Float   = wrap.constfloat
constty Double  = wrap.constdouble
