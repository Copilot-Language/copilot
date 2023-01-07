{-# LANGUAGE GADTs #-}

-- | Translate Copilot Core expressions and operators to C99.
module Copilot.Compile.C99.Translate where

import           Control.Monad.State
import qualified Data.List.NonEmpty  as NonEmpty

import Copilot.Core
import Copilot.Compile.C99.Error (impossible)
import Copilot.Compile.C99.Util

import qualified Language.C99.Simple as C

-- | Translates a Copilot Core expression into a C99 expression.
transexpr :: Expr a -> State FunEnv C.Expr
transexpr (Const ty x) = return $ constty ty x

transexpr (Local ty1 _ name e1 e2) = do
  e1' <- transexpr e1
  let cty1 = transLocalVarDeclType ty1
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
transop1 op e =
  -- There are three types of ways in which a function in Copilot Core can be
  -- translated into C:
  --
  -- 1) Direct translation (perfect 1-to-1 mapping)
  -- 2) Type-directed translation (1-to-many mapping, choice based on type)
  -- 3) Desugaring/complex (expands to complex expression)
  case op of
    Not           -> (C..!) e
    Abs      ty   -> transAbs ty e
    Sign     ty   -> transSign ty e
    Recip    ty   -> (constNumTy ty 1) C../ e
    Acos     ty   -> funcall (specializeMathFunName ty "acos") [e]
    Asin     ty   -> funcall (specializeMathFunName ty "asin") [e]
    Atan     ty   -> funcall (specializeMathFunName ty "atan") [e]
    Cos      ty   -> funcall (specializeMathFunName ty "cos") [e]
    Sin      ty   -> funcall (specializeMathFunName ty "sin") [e]
    Tan      ty   -> funcall (specializeMathFunName ty "tan") [e]
    Acosh    ty   -> funcall (specializeMathFunName ty "acosh") [e]
    Asinh    ty   -> funcall (specializeMathFunName ty "asinh") [e]
    Atanh    ty   -> funcall (specializeMathFunName ty "atanh") [e]
    Cosh     ty   -> funcall (specializeMathFunName ty "cosh") [e]
    Sinh     ty   -> funcall (specializeMathFunName ty "sinh") [e]
    Tanh     ty   -> funcall (specializeMathFunName ty "tanh") [e]
    Exp      ty   -> funcall (specializeMathFunName ty "exp") [e]
    Log      ty   -> funcall (specializeMathFunName ty "log") [e]
    Sqrt     ty   -> funcall (specializeMathFunName ty "sqrt") [e]
    Ceiling  ty   -> funcall (specializeMathFunName ty "ceil") [e]
    Floor    ty   -> funcall (specializeMathFunName ty "floor") [e]
    BwNot    _    -> (C..~) e
    Cast     _ ty -> C.Cast (transtypename ty) e
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
  Pow      ty  -> funcall (specializeMathFunName ty "pow") [e1, e2]
  Logb     ty  -> funcall (specializeMathFunName ty "log") [e2] C../
                  funcall (specializeMathFunName ty "log") [e1]
  Atan2    ty  -> funcall (specializeMathFunName ty "atan2") [e1, e2]
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

-- | Translate @'Abs' e@ in Copilot Core into a C99 expression.
--
-- This function produces a portable implementation of abs in C99 that works
-- for the type given, provided that the output fits in a variable of the same
-- type (which may not be true, for example, for signed integers in the lower
-- end of their type range). If the absolute value is out of range, the
-- behavior is undefined.
--
-- PRE: The type given is a Num type (floating-point number, or a
-- signed/unsigned integer of fixed size).
transAbs :: Type a -> C.Expr -> C.Expr
transAbs ty e
    -- Abs for floats/doubles is called fabs in C99's math.h.
    | typeIsFloating ty
    = funcall (specializeMathFunName ty "fabs") [e]

    -- C99 provides multiple implementations of abs, depending on the type of
    -- the arguments. For integers, it provides C99 abs, labs, and llabs, which
    -- take, respectively, an int, a long int, and a long long int.
    --
    -- However, the code produced by Copilot uses types with fixed width (e.g.,
    -- int16_t), and there is no guarantee that, for example, 32-bit int or
    -- 64-bit int will fit in a C int (only guaranteed to be 16 bits).
    -- Consequently, this function provides a portable version of abs for signed
    -- and unsigned ints implemented using shift and xor. For example, for a
    -- value x of type int32_t, the absolute value is:
    -- (x + (x >> sizeof(int32_t)-1)) ^ (x >> sizeof(int32_t)-1))
    | otherwise
    = (e C..+ (e C..>> tyBitSizeMinus1)) C..^ (e C..>> tyBitSizeMinus1)
  where
    -- Size of an integer type in bits, minus one. It's easier to hard-code
    -- them than to try and generate the right expressions in C using sizeof.
    --
    -- PRE: the type 'ty' is a signed or unsigned integer type.
    tyBitSizeMinus1 :: C.Expr
    tyBitSizeMinus1 = case ty of
      Int8   -> C.LitInt 7
      Int16  -> C.LitInt 15
      Int32  -> C.LitInt 31
      Int64  -> C.LitInt 63
      Word8  -> C.LitInt 7
      Word16 -> C.LitInt 15
      Word32 -> C.LitInt 31
      Word64 -> C.LitInt 63
      _      -> impossible
                  "transAbs"
                  "copilot-c99"
                  "Abs applied to unexpected types."

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
    C.InitVal (transtypename ty) (constStruct (toValues v))
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
  Array ty' -> C.InitList $ constarray ty' $ arrayelems val

  -- We use InitArray to initialize a struct because the syntax used for
  -- initializing arrays and structs is compatible. For instance, {1, 2} works
  -- both for initializing an int array of length 2 as well as a struct with
  -- two int fields, although the two expressions are conceptually different
  -- (structs can also be initialized as { .a = 1, .b = 2}.
  Struct _  -> C.InitList $ constStruct (toValues val)
  _         -> C.InitExpr $ constty ty val

-- | Transform a Copilot Core struct field into a C99 initializer.
constfieldinit :: Value a -> C.InitItem
constfieldinit (Value ty (Field val)) = C.InitItem Nothing $ constinit ty val

-- | Transform a Copilot Struct, based on the struct fields, into a list of C99
-- initializer values.
constStruct :: [Value a] -> NonEmpty.NonEmpty C.InitItem
constStruct val = NonEmpty.fromList $ map constfieldinit val

-- | Transform a Copilot Array, based on the element values and their type,
-- into a list of C99 initializer values.
constarray :: Type a -> [a] -> NonEmpty.NonEmpty C.InitItem
constarray ty =
  NonEmpty.fromList . map (C.InitItem Nothing . constinit ty)

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

-- | Translate a Copilot type to a valid (local) variable declaration C99 type.
--
-- If the type denotes an array, translate it to a pointer to whatever the
-- array holds. This special case is needed when the type is used for a local
-- variable declaration. We treat global variables differently (we generate
-- list initializers).
transLocalVarDeclType :: Type a -> C.Type
transLocalVarDeclType (Array ty') = C.Ptr $ transtype ty'
transLocalVarDeclType ty          = transtype ty

-- | Translate a Copilot type intro a C typename
transtypename :: Type a -> C.TypeName
transtypename ty = C.TypeName $ transtype ty

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

-- | Provide a specialized function name in C99 for a function given the type
-- of its arguments, and its "family" name.
--
-- C99 provides multiple variants of the same conceptual function, based on the
-- types. Depending on the function, common variants exist for signed/unsigned
-- arguments, long or short types, float or double. The C99 standard uses the
-- same mechanism to name most such functions: the default variant works for
-- double, and there are additional variants for float and long double. For
-- example, the sin function operates on double, while sinf operates on float,
-- and sinl operates on long double.
--
-- This function only knows how to provide specialized names for functions in
-- math.h that provide a default version for a double argument and vary for
-- floats. It won't change the function name given if the variation is based on
-- the return type, if the function is defined elsewhere, or for other types.
specializeMathFunName :: Type a -> String -> String
specializeMathFunName ty s
    -- The following function pattern matches based on the variants available
    -- for a specific function.
    --
    -- Do not assume that a function you need implemented follows the same
    -- standard as others: check whether it is present in the standard.
    | isMathFPArgs s
    , Float <- ty
    = s ++ "f"

    | otherwise
    = s
  where
    -- True if the function family name is part of math.h and follows the
    -- standard rule of providing multiple variants for floating point numbers
    -- based on the type of their arguments.
    --
    -- Note: nan is not in this list because the names of its variants are
    -- determined by the return type.
    --
    -- For details, see:
    -- "B.11 Mathematics <math.h>" in the C99 standard
    isMathFPArgs :: String -> Bool
    isMathFPArgs = flip elem
       [ "acos",   "asin",     "atan",      "atan2",      "cos",    "sin"
       , "tan",    "acosh",    "asinh",     "atanh",      "cosh",   "sinh"
       , "tanh",   "exp",      "exp2",      "expm1",      "frexp",  "ilogb"
       , "ldexp",  "log",      "log10",     "log1p",      "log2",   "logb"
       , "modf",   "scalbn",   "scalbln",   "cbrt",       "fabs",   "hypot"
       , "pow",    "sqrt",     "erf",       "erfc",       "lgamma", "tgamma"
       , "ceil",   "floor",    "nearbyint", "rint",       "lrint",  "llrint"
       , "round",  "lround",   "llround",   "trunc",      "fmod",   "remainder"
       , "remquo", "copysign", "nextafter", "nexttoward", "fdim"
       , "fmax",   "fmin",     "fma"
       ]

-- * Auxiliary functions

-- | True if the type given is a floating point number.
typeIsFloating :: Type a -> Bool
typeIsFloating Float  = True
typeIsFloating Double = True
typeIsFloating _      = False
