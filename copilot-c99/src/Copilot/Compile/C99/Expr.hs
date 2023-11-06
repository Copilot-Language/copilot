{-# LANGUAGE GADTs #-}

-- | Translate Copilot Core expressions and operators to C99.
module Copilot.Compile.C99.Expr
    ( transExpr
    , constArray
    )
  where

-- External imports
import           Control.Monad.State ( State, modify )
import qualified Data.List.NonEmpty  as NonEmpty
import qualified Language.C99.Simple as C

-- Internal imports: Copilot
import Copilot.Core ( Expr (..), Field (..), Op1 (..), Op2 (..), Op3 (..),
                      Type (..), Value (..), accessorName, arrayElems,
                      toValues )

-- Internal imports
import Copilot.Compile.C99.Error ( impossible )
import Copilot.Compile.C99.Name  ( exCpyName, streamAccessorName )
import Copilot.Compile.C99.Type  ( transLocalVarDeclType, transTypeName )

-- | Translates a Copilot Core expression into a C99 expression.
transExpr :: Expr a -> State FunEnv C.Expr
transExpr (Const ty x) = return $ constTy ty x

transExpr (Local ty1 _ name e1 e2) = do
  e1' <- transExpr e1
  let cTy1     = transLocalVarDeclType ty1
      initExpr = Just $ C.InitExpr e1'

  -- Add new decl to the tail of the fun env
  modify (++ [C.VarDecln Nothing cTy1 name initExpr])

  transExpr e2

transExpr (Var _ n) = return $ C.Ident n

transExpr (Drop _ amount sId) = do
  let accessVar = streamAccessorName sId
      index     = C.LitInt (fromIntegral amount)
  return $ funCall accessVar [index]

transExpr (ExternVar _ name _) = return $ C.Ident (exCpyName name)

transExpr (Label _ _ e) = transExpr e -- ignore label

transExpr (Op1 op e) = do
  e' <- transExpr e
  return $ transOp1 op e'

transExpr (Op2 op e1 e2) = do
  e1' <- transExpr e1
  e2' <- transExpr e2
  return $ transOp2 op e1' e2'

transExpr (Op3 op e1 e2 e3) = do
  e1' <- transExpr e1
  e2' <- transExpr e2
  e3' <- transExpr e3
  return $ transOp3 op e1' e2' e3'

-- | Translates a Copilot unary operator and its argument into a C99
-- expression.
transOp1 :: Op1 a b -> C.Expr -> C.Expr
transOp1 op e =
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
    Recip    ty   -> constNumTy ty 1 C../ e
    Acos     ty   -> funCall (specializeMathFunName ty "acos") [e]
    Asin     ty   -> funCall (specializeMathFunName ty "asin") [e]
    Atan     ty   -> funCall (specializeMathFunName ty "atan") [e]
    Cos      ty   -> funCall (specializeMathFunName ty "cos") [e]
    Sin      ty   -> funCall (specializeMathFunName ty "sin") [e]
    Tan      ty   -> funCall (specializeMathFunName ty "tan") [e]
    Acosh    ty   -> funCall (specializeMathFunName ty "acosh") [e]
    Asinh    ty   -> funCall (specializeMathFunName ty "asinh") [e]
    Atanh    ty   -> funCall (specializeMathFunName ty "atanh") [e]
    Cosh     ty   -> funCall (specializeMathFunName ty "cosh") [e]
    Sinh     ty   -> funCall (specializeMathFunName ty "sinh") [e]
    Tanh     ty   -> funCall (specializeMathFunName ty "tanh") [e]
    Exp      ty   -> funCall (specializeMathFunName ty "exp") [e]
    Log      ty   -> funCall (specializeMathFunName ty "log") [e]
    Sqrt     ty   -> funCall (specializeMathFunName ty "sqrt") [e]
    Ceiling  ty   -> funCall (specializeMathFunName ty "ceil") [e]
    Floor    ty   -> funCall (specializeMathFunName ty "floor") [e]
    BwNot    _    -> (C..~) e
    Cast     _ ty -> C.Cast (transTypeName ty) e
    GetField (Struct _)  _ f -> C.Dot e (accessorName f)

-- | Translates a Copilot binary operator and its arguments into a C99
-- expression.
transOp2 :: Op2 a b c -> C.Expr -> C.Expr -> C.Expr
transOp2 op e1 e2 = case op of
  And          -> e1 C..&& e2
  Or           -> e1 C..|| e2
  Add      _   -> e1 C..+  e2
  Sub      _   -> e1 C..-  e2
  Mul      _   -> e1 C..*  e2
  Mod      _   -> e1 C..%  e2
  Div      _   -> e1 C../  e2
  Fdiv     _   -> e1 C../  e2
  Pow      ty  -> funCall (specializeMathFunName ty "pow") [e1, e2]
  Logb     ty  -> funCall (specializeMathFunName ty "log") [e2] C../
                  funCall (specializeMathFunName ty "log") [e1]
  Atan2    ty  -> funCall (specializeMathFunName ty "atan2") [e1, e2]
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
transOp3 :: Op3 a b c d -> C.Expr -> C.Expr -> C.Expr -> C.Expr
transOp3 op e1 e2 e3 = case op of
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
    = funCall (specializeMathFunName ty "fabs") [e]

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
constTy :: Type a -> a -> C.Expr
constTy ty = case ty of
  Bool      -> C.LitBool
  Int8      -> explicitTy ty . C.LitInt . fromIntegral
  Int16     -> explicitTy ty . C.LitInt . fromIntegral
  Int32     -> explicitTy ty . C.LitInt . fromIntegral
  Int64     -> explicitTy ty . C.LitInt . fromIntegral
  Word8     -> explicitTy ty . C.LitInt . fromIntegral
  Word16    -> explicitTy ty . C.LitInt . fromIntegral
  Word32    -> explicitTy ty . C.LitInt . fromIntegral
  Word64    -> explicitTy ty . C.LitInt . fromIntegral
  Float     -> explicitTy ty . C.LitFloat
  Double    -> explicitTy ty . C.LitDouble
  Struct _  -> C.InitVal (transTypeName ty) . constStruct . toValues
  Array ty' -> C.InitVal (transTypeName ty) . constArray ty' . arrayElems

-- | Transform a Copilot Core literal, based on its value and type, into a C99
-- initializer.
constInit :: Type a -> a -> C.Init
constInit ty val = case ty of
  -- We include two special cases for Struct and Array to avoid using constTy
  -- on them.
  --
  -- In the default case (i.e., InitExpr (constTy ty val)), constant
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
  Array ty' -> C.InitList $ constArray ty' $ arrayElems val

  -- We use InitArray to initialize a struct because the syntax used for
  -- initializing arrays and structs is compatible. For instance, {1, 2} works
  -- both for initializing an int array of length 2 as well as a struct with
  -- two int fields, although the two expressions are conceptually different
  -- (structs can also be initialized as { .a = 1, .b = 2}.
  Struct _  -> C.InitList $ constStruct (toValues val)
  _         -> C.InitExpr $ constTy ty val

-- | Transform a Copilot Core struct field into a C99 initializer.
constFieldInit :: Value a -> C.InitItem
constFieldInit (Value ty (Field val)) = C.InitItem Nothing $ constInit ty val

-- | Transform a Copilot Struct, based on the struct fields, into a list of C99
-- initializer values.
constStruct :: [Value a] -> NonEmpty.NonEmpty C.InitItem
constStruct val = NonEmpty.fromList $ map constFieldInit val

-- | Transform a Copilot Array, based on the element values and their type,
-- into a list of C99 initializer values.
constArray :: Type a -> [a] -> NonEmpty.NonEmpty C.InitItem
constArray ty =
  NonEmpty.fromList . map (C.InitItem Nothing . constInit ty)

-- | Explicitly cast a C99 value to a type.
explicitTy :: Type a -> C.Expr -> C.Expr
explicitTy ty = C.Cast (transTypeName ty)

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

-- | Auxiliary type used to collect all the declarations of all the variables
-- used in a function to be generated, since variable declarations are always
-- listed first at the top of the function body.
type FunEnv = [C.Decln]

-- | Define a C expression that calls a function with arguments.
funCall :: C.Ident   -- ^ Function name
        -> [C.Expr]  -- ^ Arguments
        -> C.Expr
funCall name = C.Funcall (C.Ident name)
