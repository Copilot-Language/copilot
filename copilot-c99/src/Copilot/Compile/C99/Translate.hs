{-# LANGUAGE GADTs #-}

-- | Translate Copilot Core expressions and operators to C99.
module Copilot.Compile.C99.Translate where

import Control.Monad.State

import Copilot.Core
import Copilot.Compile.C99.Util

import qualified Language.C99.Simple as C

-- | Translates a Copilot Core expression into a C99 expression.
transexpr :: Expr a -> State FunEnv C.Expr
transexpr (Const ty x) = return $ constty ty x

transexpr (Local ty1 _ name e1 e2) = do
  e1' <- transexpr e1
  let cty1 = transtype ty1
      init = Just $ C.InitExpr e1'
  statetell [C.VarDecln Nothing cty1 name init]

  transexpr e2

transexpr (Var _ n) = return $ C.Ident n

transexpr (Drop _ amount sid) = do
  let accessvar = streamaccessorname sid
      index     = C.LitInt (fromIntegral amount)
  return $ funcall accessvar [index]

transexpr (ExternVar _ name _) = return $ C.Ident (excpyname name)

transexpr (Label _ _ e) = transexpr e -- ignore label

transexpr (Op1 op e) = do
  e' <- transexpr e
  return $ transop1 op e'

transexpr (Op2 op e1 e2) = do
  e1' <- transexpr e1
  e2' <- transexpr e2
  return $ transop2 op e1' e2'

transexpr (Op3 op e1 e2 e3) = do
  e1' <- transexpr e1
  e2' <- transexpr e2
  e3' <- transexpr e3
  return $ transop3 op e1' e2' e3'


-- | Translates a Copilot unary operator and its argument into a C99
-- expression.
transop1 :: Op1 a b -> C.Expr -> C.Expr
transop1 op e = case op of
  Not             -> (C..!) e
  Abs      ty     -> transabs ty e
  Sign     _      -> funcall "copysign" [C.LitDouble 1.0, e]
  Recip    ty     -> transrecip ty e
  Exp      ty     -> transopFP "exp"   ty [e]
  Sqrt     ty     -> transopFP "sqrt"  ty [e]
  Log      ty     -> transopFP "log"   ty [e]
  Sin      ty     -> transopFP "sin"   ty [e]
  Tan      ty     -> transopFP "tan"   ty [e]
  Cos      ty     -> transopFP "cos"   ty [e]
  Asin     ty     -> transopFP "asin"  ty [e]
  Atan     ty     -> transopFP "atan"  ty [e]
  Acos     ty     -> transopFP "acos"  ty [e]
  Sinh     ty     -> transopFP "sinh"  ty [e]
  Tanh     ty     -> transopFP "tanh"  ty [e]
  Cosh     ty     -> transopFP "cosh"  ty [e]
  Asinh    ty     -> transopFP "asinh" ty [e]
  Atanh    ty     -> transopFP "atanh" ty [e]
  Acosh    ty     -> transopFP "acosh" ty [e]
  Ceiling  ty     -> transopFP "ceil"  ty [e]
  Floor    ty     -> transopFP "floor" ty [e]
  BwNot    _      -> (C..~) e
  Cast     _ ty  -> C.Cast (transtypename ty) e
  GetField (Struct _)  _ f -> C.Dot e (accessorname f)

-- | Translates a Copilot binary operator and its arguments into a C99
-- expression.
transop2 :: Op2 a b c -> C.Expr -> C.Expr -> C.Expr
transop2 op e1 e2 = case op of
  And          -> e1 C..&& e2
  Or           -> e1 C..|| e2
  Add      _   -> e1 C..+  e2
  Sub      _   -> e1 C..-  e2
  Mul      _   -> e1 C..*  e2
  Mod      _   -> e1 C..%  e2
  Div      _   -> e1 C../  e2
  Fdiv     _   -> e1 C../  e2
  Pow      ty  -> transopFP "pow" ty [e1, e2]
  Logb     ty  -> transopFP "log" ty [e2] C../ transopFP "log" ty [e1]
  Atan2    ty  -> transopFP "atan2" ty [e1, e2]
  Eq       _   -> e1 C..== e2
  Ne       _   -> e1 C..!= e2
  Le       _   -> e1 C..<= e2
  Ge       _   -> e1 C..>= e2
  Lt       _   -> e1 C..<  e2
  Gt       _   -> e1 C..>  e2
  BwAnd    _   -> e1 C..&  e2
  BwOr     _   -> e1 C..|  e2
  BwXor    _   -> e1 C..^  e2
  BwShiftL _ _ -> e1 C..<< e2
  BwShiftR _ _ -> e1 C..>> e2
  Index    _   -> C.Index e1 e2

-- | Translates a Copilot ternary operator and its arguments into a C99
-- expression.
transop3 :: Op3 a b c d -> C.Expr -> C.Expr -> C.Expr -> C.Expr
transop3 op e1 e2 e3 = case op of
  Mux _ -> C.Cond e1 e2 e3

-- | Translates a Copilot 'Abs' operator and its argument into a C99
-- expression.
transabs :: Type a -> C.Expr -> C.Expr
-- NB: Don't implement this in terms of transopFP, as Abs has a sensible
-- translation for integral types.
transabs ty e = case ty of
  Float  -> funcall "fabsf" [e]
  Double -> funcall "fabs"  [e]
  _      -> funcall "abs"   [e]

-- | Translates a Copilot 'Recip' operator and its argument into a C99
-- expression.
transrecip :: Type a -> C.Expr -> C.Expr
transrecip ty e = case ty of
  Float  -> C.LitFloat  1.0 C../ e
  Double -> C.LitDouble 1.0 C../ e
  _      -> error "Recip only supported for floating-point values"

-- | Translates a Copilot operator that takes floating-point arguments into a
-- C99 expression. This function will error if given a 'Type' besides 'Double'
-- or 'Float'.
transopFP ::
     String   -- ^ The prefix to use for the name of the underlying operation
              --   in @math.h@. For example, if translating a call to @atan2@
              --   (for @double@s) or @atan2f@ (for @float@s), pass @\"atan2\"@
              --   as the prefix. This function will take care of picking
              --   between @atan2@/@atan2f@ depending on which 'Type' is
              --   supplied.
  -> Type a   -- ^ The type of the arguments.
  -> [C.Expr] -- ^ The argument values.
  -> C.Expr
transopFP cFunPrefix ty args = case ty of
  -- If the argument types are doubles, use the original C function name without
  -- a suffix.
  Double -> funcall cFunPrefix args
  -- If the argument types are floats, attach an `f` suffix to the name to pick
  -- the float-specific version of the function.
  Float -> funcall (cFunPrefix ++ "f") args
  -- If the argument types are anything else, throw an error.
  _ -> error $ cFunPrefix ++ " only supported for floating-point values"

-- | Transform a Copilot Core literal, based on its value and type, into a C99
-- literal.
constty :: Type a -> a -> C.Expr
constty ty = case ty of
  Bool   -> C.LitBool
  Int8   -> explicitty ty . C.LitInt . fromIntegral
  Int16  -> explicitty ty . C.LitInt . fromIntegral
  Int32  -> explicitty ty . C.LitInt . fromIntegral
  Int64  -> explicitty ty . C.LitInt . fromIntegral
  Word8  -> explicitty ty . C.LitInt . fromIntegral
  Word16 -> explicitty ty . C.LitInt . fromIntegral
  Word32 -> explicitty ty . C.LitInt . fromIntegral
  Word64 -> explicitty ty . C.LitInt . fromIntegral
  Float  -> explicitty ty . C.LitFloat
  Double -> explicitty ty . C.LitDouble
  Struct _ -> \v -> C.InitVal (transtypename ty) (map fieldinit (toValues v))
    where
      fieldinit (Value ty (Field val)) = C.InitExpr $ constty ty val
  Array ty' -> \v -> C.InitVal (transtypename ty) (vals v)
    where
      vals v = constarray ty' (arrayelems v)

      constarray :: Type a -> [a] -> [C.Init]
      constarray ty xs = case ty of
        Array ty' -> constarray ty' (concatMap arrayelems xs)
        _         -> map (C.InitExpr . constty ty) xs


-- | Explicitly cast a C99 value to a type.
explicitty :: Type a -> C.Expr -> C.Expr
explicitty ty = C.Cast (transtypename ty)

-- | Translate a Copilot type to a C99 type.
transtype :: Type a -> C.Type
transtype ty = case ty of
  Bool      -> C.TypeSpec $ C.TypedefName "bool"
  Int8      -> C.TypeSpec $ C.TypedefName "int8_t"
  Int16     -> C.TypeSpec $ C.TypedefName "int16_t"
  Int32     -> C.TypeSpec $ C.TypedefName "int32_t"
  Int64     -> C.TypeSpec $ C.TypedefName "int64_t"
  Word8     -> C.TypeSpec $ C.TypedefName "uint8_t"
  Word16    -> C.TypeSpec $ C.TypedefName "uint16_t"
  Word32    -> C.TypeSpec $ C.TypedefName "uint32_t"
  Word64    -> C.TypeSpec $ C.TypedefName "uint64_t"
  Float     -> C.TypeSpec C.Float
  Double    -> C.TypeSpec C.Double
  Array ty' -> C.Array (transtype ty') length where
    length = Just $ C.LitInt $ fromIntegral $ tylength ty
  Struct s  -> C.TypeSpec $ C.Struct (typename s)

-- | Translate a Copilot type intro a C typename
transtypename :: Type a -> C.TypeName
transtypename ty = C.TypeName $ transtype ty
