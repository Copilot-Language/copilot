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
  Abs      _      -> funcall "abs"      [e]
  Sign     ty     -> transSign ty e
  Recip    _      -> C.LitDouble 1.0 C../ e
  Exp      _      -> funcall "exp"   [e]
  Sqrt     _      -> funcall "sqrt"  [e]
  Log      _      -> funcall "log"   [e]
  Sin      _      -> funcall "sin"   [e]
  Tan      _      -> funcall "tan"   [e]
  Cos      _      -> funcall "cos"   [e]
  Asin     _      -> funcall "asin"  [e]
  Atan     _      -> funcall "atan"  [e]
  Acos     _      -> funcall "acos"  [e]
  Sinh     _      -> funcall "sinh"  [e]
  Tanh     _      -> funcall "tanh"  [e]
  Cosh     _      -> funcall "cosh"  [e]
  Asinh    _      -> funcall "asinh" [e]
  Atanh    _      -> funcall "atanh" [e]
  Acosh    _      -> funcall "acosh" [e]
  Ceiling  _      -> funcall "ceil"  [e]
  Floor    _      -> funcall "floor" [e]
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
  Pow      _   -> funcall "pow" [e1, e2]
  Logb     _   -> funcall "log" [e2] C../ funcall "log" [e1]
  Atan2    _   -> funcall "atan2" [e1, e2]
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

-- | Translate @'Sign' e@ in Copilot Core into a C99 expression.
--
-- Sign is is translated as @e > 0 ? 1 : (e < 0 ? -1 : e)@, that is:
--
-- 1. If @e@ is positive, return @1@.
--
-- 2. If @e@ is negative, return @-1@.
--
-- 3. Otherwise, return @e@. This handles the case where @e@ is @0@ when the
--    type is an integral type. If the type is a floating-point type, it also
--    handles the cases where @e@ is @-0@ or @NaN@.
--
-- This implementation is modeled after how GHC implements 'signum'
-- <https://gitlab.haskell.org/ghc/ghc/-/blob/aed98ddaf72cc38fb570d8415cac5de9d8888818/libraries/base/GHC/Float.hs#L523-L525 here>.
transSign :: Type a -> C.Expr -> C.Expr
transSign ty e = positiveCase $ negativeCase e
  where
    -- If @e@ is positive, return @1@, otherwise fall back to argument.
    --
    -- Produces the following code, where @<arg>@ is the argument to this
    -- function:
    -- @
    -- e > 0 ? 1 : <arg>
    -- @
    positiveCase :: C.Expr  -- ^ Value returned if @e@ is not positive.
                 -> C.Expr
    positiveCase =
      C.Cond (C.BinaryOp C.GT e (constNumTy ty 0)) (constNumTy ty 1)

    -- If @e@ is negative, return @1@, otherwise fall back to argument.
    --
    -- Produces the following code, where @<arg>@ is the argument to this
    -- function:
    -- @
    -- e < 0 ? -1 : <arg>
    -- @
    negativeCase :: C.Expr  -- ^ Value returned if @e@ is not negative.
                 -> C.Expr
    negativeCase =
      C.Cond (C.BinaryOp C.LT e (constNumTy ty 0)) (constNumTy ty (-1))

    -- Translate a literal number of type @ty@ into a C99 literal.
    --
    -- PRE: The type of PRE is numeric (integer or floating-point), that
    -- is, not boolean, struct or array.
    constNumTy :: Type a -> Integer -> C.Expr
    constNumTy ty =
      case ty of
        Float  -> C.LitFloat . fromInteger
        Double -> C.LitDouble . fromInteger
        _      -> C.LitInt

-- | Transform a Copilot Core literal, based on its value and type, into a C99
-- literal.
constty :: Type a -> a -> C.Expr
constty ty = case ty of
  Bool      -> C.LitBool
  Int8      -> explicitty ty . C.LitInt . fromIntegral
  Int16     -> explicitty ty . C.LitInt . fromIntegral
  Int32     -> explicitty ty . C.LitInt . fromIntegral
  Int64     -> explicitty ty . C.LitInt . fromIntegral
  Word8     -> explicitty ty . C.LitInt . fromIntegral
  Word16    -> explicitty ty . C.LitInt . fromIntegral
  Word32    -> explicitty ty . C.LitInt . fromIntegral
  Word64    -> explicitty ty . C.LitInt . fromIntegral
  Float     -> explicitty ty . C.LitFloat
  Double    -> explicitty ty . C.LitDouble
  Struct _  -> \v ->
    C.InitVal (transtypename ty) (map constfieldinit (toValues v))
  Array ty' -> \v ->
    C.InitVal (transtypename ty) (constarray ty' (arrayelems v))

-- | Transform a Copilot Core literal, based on its value and type, into a C99
-- initializer.
constinit :: Type a -> a -> C.Init
constinit ty val = case ty of
  -- We include two special cases for Struct and Array to avoid using constty
  -- on them.
  --
  -- In the default case (i.e., InitExpr (constty ty val)), constant
  -- initializations are explicitly cast. However, doing so 1) may result in
  -- incorrect values for arrays, and 2) will be considered a non-constant
  -- expression in the case of arrays and structs, and thus not allowed as the
  -- initialization value for a global variable.
  --
  -- In particular, wrt. (1), for example, the nested array:
  --   [[0, 1], [2, 3]] :: Array 2 (Array 2 Int32)
  --
  -- with explicit casts, will be initialized in C as:
  --   { (int32_t[2]){(int32_t)(0), (int32_t)(1)},
  --     (int32_t[2]){(int32_t)(2), (int32_t)(3)} }
  --
  -- Due to the additional (int32_t[2]) casts, a C compiler will interpret the
  -- whole expression as an array of two int32_t's (as opposed to a nested
  -- array). This can either lead to compile-time errors (if you're lucky) or
  -- incorrect runtime semantics (if you're unlucky).
  Array ty' -> C.InitArray $ constarray ty' $ arrayelems val

  -- We use InitArray to initialize a struct because the syntax used for
  -- initializing arrays and structs is compatible. For instance, {1, 2} works
  -- both for initializing an int array of length 2 as well as a struct with
  -- two int fields, although the two expressions are conceptually different
  -- (structs can also be initialized as { .a = 1, .b = 2}, but language-c99
  -- does not support such syntax and does not provide a specialized
  -- initialization construct for structs).
  Struct _  -> C.InitArray $ map constfieldinit $ toValues val
  _         -> C.InitExpr $ constty ty val

-- | Transform a Copilot Core struct field into a C99 initializer.
constfieldinit :: Value a -> C.Init
constfieldinit (Value ty (Field val)) = constinit ty val

-- | Transform a Copilot Array, based on the element values and their type,
-- into a list of C99 initializer values.
constarray :: Type a -> [a] -> [C.Init]
constarray ty = map (constinit ty)

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
  Array ty' -> C.Array (transtype ty') length
    where
      length = Just $ C.LitInt $ fromIntegral $ tylength ty
  Struct s  -> C.TypeSpec $ C.Struct (typename s)

-- | Translate a Copilot type intro a C typename
transtypename :: Type a -> C.TypeName
transtypename ty = C.TypeName $ transtype ty
