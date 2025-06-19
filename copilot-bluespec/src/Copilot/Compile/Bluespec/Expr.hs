{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Translate Copilot Core expressions and operators to Bluespec.
module Copilot.Compile.Bluespec.Expr
  ( transExpr
  , cIndexVector
  , cLit
  , constTy
  , genVector
  ) where

-- External imports
import Data.Foldable (foldl')
import Data.String (IsString (..))
import qualified Language.Bluespec.Classic.AST as BS
import qualified Language.Bluespec.Classic.AST.Builtin.Ids as BS

-- Internal imports: Copilot
import Copilot.Core

-- Internal imports
import Copilot.Compile.Bluespec.Error (impossible)
import Copilot.Compile.Bluespec.Name
import Copilot.Compile.Bluespec.Type

-- | Translates a Copilot Core expression into a Bluespec expression.
transExpr :: Expr a -> BS.CExpr
transExpr (Const ty x) = constTy ty x

transExpr (Local ty1 _ name e1 e2) =
  let nameId = BS.mkId BS.NoPos $ fromString name
      e1'    = transExpr e1
      ty1'   = transType ty1
      e2'    = transExpr e2 in
  BS.Cletrec
    [ BS.CLValueSign
        (BS.CDef nameId (BS.CQType [] ty1') [BS.CClause [] [] e1'])
        []
    ]
    e2'

transExpr (Var _ n) = BS.CVar $ BS.mkId BS.NoPos $ fromString n

transExpr (Drop _ amount sid) =
  let accessVar = streamAccessorName sid
      index     = BS.LInt $ BS.ilDec $ fromIntegral amount in
  BS.CApply (BS.CVar $ BS.mkId BS.NoPos $ fromString accessVar)
            [BS.CLit $ BS.CLiteral BS.NoPos index]

transExpr (ExternVar _ name _) =
  let ifcArgId = BS.mkId BS.NoPos $ fromString ifcArgName in
  BS.CSelect
    (BS.CSelect
      (BS.CVar ifcArgId)
      (BS.mkId BS.NoPos $ fromString $ lowercaseName name))
    (BS.id_read BS.NoPos)

transExpr (Label _ _ e) = transExpr e -- ignore label

transExpr (Op1 op e) = transOp1 op (transExpr e)

transExpr (Op2 op e1 e2) = transOp2 op (transExpr e1) (transExpr e2)

transExpr (Op3 op e1 e2 e3) =
  transOp3 op (transExpr e1) (transExpr e2) (transExpr e3)

-- | Translates a Copilot unary operator and its argument into a Bluespec
-- function.
transOp1 :: Op1 a b -> BS.CExpr -> BS.CExpr
transOp1 op e =
  case op of
    Not         -> app BS.idNot
    Abs     _ty -> app $ BS.mkId BS.NoPos "abs"
    Sign     ty -> transSign ty e
    -- Bluespec's Arith class does not have a `recip` method corresponding to
    -- Haskell's `recip` in the `Fractional` class, so we implement it
    -- ourselves.
    Recip    ty -> BS.CApply
                     (BS.CVar (BS.idSlashAt BS.NoPos))
                     [constNumTy ty 1, e]
    BwNot   _ty -> app $ BS.idInvertAt BS.NoPos

    Cast fromTy toTy -> transCast fromTy toTy e
    GetField (Struct _)  _ f -> BS.CSelect e $ BS.mkId BS.NoPos $
                                fromString $ lowercaseName $ accessorName f
    GetField _ _ _ -> impossible "transOp1" "copilot-bluespec"

    -- BDPI-supported operations
    Sqrt    ty -> appFP ty "sqrt"
    Exp     ty -> appFP ty "exp"
    Log     ty -> appFP ty "log"
    Acos    ty -> appFP ty "acos"
    Asin    ty -> appFP ty "asin"
    Atan    ty -> appFP ty "atan"
    Cos     ty -> appFP ty "cos"
    Sin     ty -> appFP ty "sin"
    Tan     ty -> appFP ty "tan"
    Acosh   ty -> appFP ty "acosh"
    Asinh   ty -> appFP ty "asinh"
    Atanh   ty -> appFP ty "atanh"
    Cosh    ty -> appFP ty "cosh"
    Sinh    ty -> appFP ty "sinh"
    Tanh    ty -> appFP ty "tanh"
    Ceiling ty -> appFP ty "ceiling"
    Floor   ty -> appFP ty "floor"
  where
    app :: BS.Id -> BS.CExpr
    app i = BS.CApply (BS.CVar i) [e]

    appFP :: forall t. Type t -> String -> BS.CExpr
    appFP ty funPrefix = app $ fpFunId ty funPrefix

-- | Translates a Copilot binary operator and its arguments into a Bluespec
-- function.
transOp2 :: Op2 a b c -> BS.CExpr -> BS.CExpr -> BS.CExpr
transOp2 op e1 e2 =
  case op of
    And          -> app BS.idAnd
    Or           -> app $ BS.idOrAt BS.NoPos
    Add      _ty -> app BS.idPlus
    Sub      _ty -> app BS.idMinus
    Mul      _ty -> app $ BS.idStarAt BS.NoPos
    Mod      _ty -> app $ BS.idPercentAt BS.NoPos
    Div      _ty -> app $ BS.idSlashAt BS.NoPos
    Fdiv     _ty -> app $ BS.idSlashAt BS.NoPos
    Eq       _   -> app BS.idEqual
    Ne       _   -> app BS.idNotEqual
    Le       ty  -> transLe ty e1 e2
    Ge       ty  -> transGe ty e1 e2
    Lt       ty  -> transLt ty e1 e2
    Gt       ty  -> transGt ty e1 e2
    BwAnd    _   -> app $ BS.idBitAndAt BS.NoPos
    BwOr     _   -> app $ BS.idBitOrAt BS.NoPos
    BwXor    _   -> app $ BS.idCaretAt BS.NoPos
    BwShiftL _ _ -> app $ BS.idLshAt BS.NoPos
    BwShiftR _ _ -> app $ BS.idRshAt BS.NoPos
    Index    _   -> cIndexVector e1 e2
    UpdateField (Struct _) _ f ->
      let field :: BS.FString
          field = fromString $ lowercaseName $ accessorName f in
      BS.CStructUpd e1 [(BS.mkId BS.NoPos field, e2)]
    UpdateField _ _ _ -> impossible "transOp2" "copilot-bluespec"

    -- BDPI-supported operations
    Pow      ty -> appFP ty "pow"
    Logb     ty -> appFP ty "logb"
    Atan2    ty -> appFP ty "atan2"
  where
    app :: BS.Id -> BS.CExpr
    app i = BS.CApply (BS.CVar i) [e1, e2]

    appFP :: forall t. Type t -> String -> BS.CExpr
    appFP ty funPrefix = app $ fpFunId ty funPrefix

-- | Translates a Copilot ternary operator and its arguments into a Bluespec
-- function.
transOp3 :: Op3 a b c d -> BS.CExpr -> BS.CExpr -> BS.CExpr -> BS.CExpr
transOp3 op e1 e2 e3 =
  case op of
    Mux _ -> BS.Cif BS.NoPos e1 e2 e3
    UpdateArray _ -> cUpdateVector e1 e2 e3

-- | Translate @'Sign' e@ in Copilot Core into a Bluespec expression.
--
-- @signum e@ is translated as:
--
-- @
-- if e > 0 then 1 else (if e < 0 then negate 1 else e)
-- @
--
-- That is:
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
transSign :: Type a -> BS.CExpr -> BS.CExpr
transSign ty e = positiveCase $ negativeCase e
  where
    -- If @e@ is positive, return @1@, otherwise fall back to the argument.
    --
    -- Produces the following code, where @<arg>@ is the argument to this
    -- function:
    -- @
    -- if e > 0 then 1 else <arg>
    -- @
    positiveCase :: BS.CExpr  -- ^ Value returned if @e@ is not positive.
                 -> BS.CExpr
    positiveCase =
      BS.Cif BS.NoPos (transGt ty e (constNumTy ty 0)) (constNumTy ty 1)

    -- If @e@ is negative, return @1@, otherwise fall back to the argument.
    --
    -- Produces the following code, where @<arg>@ is the argument to this
    -- function:
    -- @
    -- if e < 0 then negate 1 else <arg>
    -- @
    negativeCase :: BS.CExpr  -- ^ Value returned if @e@ is not negative.
                 -> BS.CExpr
    negativeCase =
      BS.Cif BS.NoPos (transLt ty e (constNumTy ty 0)) (constNumTy ty (-1))

-- | Translate a Copilot @x < y@ expression into Bluespec. We will generate
-- different code depending on whether the arguments have a floating-point type
-- or not.
transLt :: Type a
        -- ^ The type of the arguments.
        -> BS.CExpr -> BS.CExpr -> BS.CExpr
transLt ty e1 e2
  | typeIsFloating ty
  = transLtOrGtFP (BS.mkId BS.NoPos "LT") e1 e2
  | otherwise
  = BS.CApply (BS.CVar (BS.idLtAt BS.NoPos)) [e1, e2]

-- | Translate a Copilot @x > y@ expression into Bluespec. We will generate
-- different code depending on whether the arguments have a floating-point type
-- or not.
transGt :: Type a
        -- ^ The type of the arguments.
        -> BS.CExpr -> BS.CExpr -> BS.CExpr
transGt ty e1 e2
  | typeIsFloating ty
  = transLtOrGtFP (BS.mkId BS.NoPos "GT") e1 e2
  | otherwise
  = BS.CApply (BS.CVar (BS.idGtAt BS.NoPos)) [e1, e2]

-- | Translate a Copilot @x <= y@ expression into Bluespec. We will generate
-- different code depending on whether the arguments have a floating-point type
-- or not.
transLe :: Type a
        -- ^ The type of the arguments.
        -> BS.CExpr -> BS.CExpr -> BS.CExpr
transLe ty e1 e2
  | typeIsFloating ty
  = transLeOrGeFP (BS.mkId BS.NoPos "LT") e1 e2
  | otherwise
  = BS.CApply (BS.CVar (BS.idLtEqAt BS.NoPos)) [e1, e2]

-- | Translate a Copilot @x >= y@ expression into Bluespec. We will generate
-- different code depending on whether the arguments have a floating-point type
-- or not.
transGe :: Type a
        -- ^ The type of the arguments.
        -> BS.CExpr -> BS.CExpr -> BS.CExpr
transGe ty e1 e2
  | typeIsFloating ty
  = transLeOrGeFP (BS.mkId BS.NoPos "GT") e1 e2
  | otherwise
  = BS.CApply (BS.CVar (BS.idGtEqAt BS.NoPos)) [e1, e2]

-- | Translate a Copilot floating-point comparison involving @<@ or @>@ into a
-- Bluespec expression. Specifically, @x < y@ is translated to:
--
-- @
-- compareFP x y == LT
-- @
--
-- @x > y@ is translated similarly, except that @GT@ is used instead of @LT@.
--
-- See the comments on 'compareFPExpr' for why we translate floating-point
-- comparison operators this way.
transLtOrGtFP :: BS.Id
                 -- ^ A @Disorder@ label, which we check against the result of
                 -- calling @compareFP@. This should be either @LT@ or @GT@.
              -> BS.CExpr -> BS.CExpr -> BS.CExpr
transLtOrGtFP disorderLabel e1 e2 =
  BS.CApply
    (BS.CVar BS.idEqual)
    [compareFPExpr e1 e2, BS.CCon disorderLabel []]

-- | Translate a Copilot floating-point comparison involving @<=@ or @>=@ into
-- a Bluespec expression. Specifically, @x <= y@ is translated to:
--
-- @
-- let _c = compareFP x y
-- in (_c == LT) || (_c == EQ)
-- @
--
-- @x >= y@ is translated similarly, except that @GT@ is used instead of @LT@.
--
-- See the comments on 'compareFPExpr' for why we translate floating-point
-- comparison operators this way.
transLeOrGeFP :: BS.Id
                 -- ^ A @Disorder@ label, which we check against the result of
                 -- calling @compareFP@. This should be either @LT@ or @GT@.
              -> BS.CExpr -> BS.CExpr -> BS.CExpr
transLeOrGeFP disorderLabel e1 e2 =
  BS.Cletrec
    [BS.CLValue c [BS.CClause [] [] (compareFPExpr e1 e2)] []]
    (BS.CApply
      (BS.CVar (BS.idOrAt BS.NoPos))
      [ BS.CApply
          (BS.CVar BS.idEqual)
          [BS.CVar c, BS.CCon disorderLabel []]
      , BS.CApply
          (BS.CVar BS.idEqual)
          [BS.CVar c, BS.CCon (BS.mkId BS.NoPos "EQ") []]
      ])
  where
    c = BS.mkId BS.NoPos "_c"

-- | Generate an expression of the form @compareFP x y@. This is used to power
-- the translations of the Copilot @<@, @<=@, @>@, and @>=@ floating-point
-- operators to Bluespec.
--
-- Translating these operators using @compareFP@ is a somewhat curious design
-- choice, given that Bluespec already defines its own versions of these
-- operators. Unfortunately, we cannot directly use the Bluespec versions of
-- these operators, as they are defined in such a way that they will call
-- @error@ when one of the arguments is a NaN value. This would pose two
-- problems:
--
-- 1. This would differ from the semantics of Copilot, where @x < y@ will return
--    @False@ (instead of erroring) when one of the arguments is NaN. (Similarly
--    for the other floating-point comparison operators.)
--
-- 2. Moreover, if you have a Bluespec program that calls @x < y@, where the
--    value of @x@ or @y@ is derived from a register, then @bsc@ will always
--    fail to compile the code. This is because Bluespec must generate hardware
--    for all possible code paths in @<@, and because one of the code paths
--    calls @error@, this will cause compilation to result in an error. (See
--    https://github.com/B-Lang-org/bsc/discussions/711#discussioncomment-10003586
--    for a more detailed explanation.)
--
-- As such, we avoid using Bluespec's comparison operators and instead translate
-- Copilot's comparison operators to expressions derived from @compareFP@.
-- Unlike Bluespec's other comparison operators, calling @compareFP@ will never
-- result in an error.
compareFPExpr :: BS.CExpr -> BS.CExpr -> BS.CExpr
compareFPExpr e1 e2 =
  BS.CApply
    (BS.CVar (BS.mkId BS.NoPos "compareFP"))
    [e1, e2]

-- | Bluespec does not have a general-purpose casting operation, so we must
-- handle casts on a case-by-case basis.
transCast :: Type a -> Type b -> BS.CExpr -> BS.CExpr
transCast fromTy toTy =
  case (fromTy, toTy) of
    -----
    -- "safe" casts that cannot lose information
    -----

    (Bool,  Bool)    -> id
    (Bool,  Word8)   -> upcastBool
    (Bool,  Word16)  -> upcastBool
    (Bool,  Word32)  -> upcastBool
    (Bool,  Word64)  -> upcastBool
    (Bool,  Int8)    -> upcastBool
    (Bool,  Int16)   -> upcastBool
    (Bool,  Int32)   -> upcastBool
    (Bool,  Int64)   -> upcastBool

    (Int8,  Int8)    -> id
    (Int8,  Int16)   -> upcast
    (Int8,  Int32)   -> upcast
    (Int8,  Int64)   -> upcast
    (Int16, Int16)   -> id
    (Int16, Int32)   -> upcast
    (Int16, Int64)   -> upcast
    (Int32, Int32)   -> id
    (Int32, Int64)   -> upcast
    (Int64, Int64)   -> id

    (Word8,  Int16)  -> unpackPackUpcast Word16
    (Word8,  Int32)  -> unpackPackUpcast Word32
    (Word8,  Int64)  -> unpackPackUpcast Word64
    (Word8,  Word8)  -> id
    (Word8,  Word16) -> upcast
    (Word8,  Word32) -> upcast
    (Word8,  Word64) -> upcast
    (Word16, Int32)  -> unpackPackUpcast Word32
    (Word16, Int64)  -> unpackPackUpcast Word64
    (Word16, Word16) -> id
    (Word16, Word32) -> upcast
    (Word16, Word64) -> upcast
    (Word32, Int64)  -> unpackPackUpcast Word64
    (Word32, Word32) -> id
    (Word32, Word64) -> upcast
    (Word64, Word64) -> id

    -----
    -- "unsafe" casts, which may lose information
    -----

    -- unsigned truncations
    (Word64, Word32) -> downcast
    (Word64, Word16) -> downcast
    (Word64, Word8)  -> downcast
    (Word32, Word16) -> downcast
    (Word32, Word8)  -> downcast
    (Word16, Word8)  -> downcast

    -- signed truncations
    (Int64, Int32)   -> downcast
    (Int64, Int16)   -> downcast
    (Int64, Int8)    -> downcast
    (Int32, Int16)   -> downcast
    (Int32, Int8)    -> downcast
    (Int16, Int8)    -> downcast

    -- signed integer to float
    (Int64, Float)   -> castIntegralToFloatingPoint
    (Int32, Float)   -> castIntegralToFloatingPoint
    (Int16, Float)   -> castIntegralToFloatingPoint
    (Int8,  Float)   -> castIntegralToFloatingPoint

    -- unsigned integer to float
    (Word64, Float)  -> castIntegralToFloatingPoint
    (Word32, Float)  -> castIntegralToFloatingPoint
    (Word16, Float)  -> castIntegralToFloatingPoint
    (Word8,  Float)  -> castIntegralToFloatingPoint

    -- signed integer to double
    (Int64, Double)  -> castIntegralToFloatingPoint
    (Int32, Double)  -> castIntegralToFloatingPoint
    (Int16, Double)  -> castIntegralToFloatingPoint
    (Int8,  Double)  -> castIntegralToFloatingPoint

    -- unsigned integer to double
    (Word64, Double) -> castIntegralToFloatingPoint
    (Word32, Double) -> castIntegralToFloatingPoint
    (Word16, Double) -> castIntegralToFloatingPoint
    (Word8,  Double) -> castIntegralToFloatingPoint

    -- unsigned to signed conversion
    (Word64, Int64)  -> unpackPack
    (Word32, Int32)  -> unpackPack
    (Word16, Int16)  -> unpackPack
    (Word8,  Int8)   -> unpackPack

    -- signed to unsigned conversion
    (Int64, Word64)  -> unpackPack
    (Int32, Word32)  -> unpackPack
    (Int16, Word16)  -> unpackPack
    (Int8,  Word8)   -> unpackPack

    _ -> impossible "transCast" "copilot-bluespec"
  where
    -- unpackPack :: (Bits fromTy n, Bits toTy n) => fromTy -> toTy
    -- unpackPack e = (unpack (pack e)) :: toTy
    --
    -- The most basic cast. Used when fromTy and toTy are both integral types
    -- with the same number of bits.
    unpackPack :: BS.CExpr -> BS.CExpr
    unpackPack e = withTypeAnnotation toTy $
                   BS.CApply
                     (BS.CVar BS.idUnpack)
                     [BS.CApply (BS.CVar BS.idPack) [e]]

    -- upcastBool :: (Add k 1 n, Bits toTy n) => Bool -> toTy
    -- upcastBool b = (unpack (extend (pack b))) :: toTy
    --
    -- Cast a Bool to a `Bits 1` value, extend it to `Bits n`, and then
    -- convert it back to an integral type.
    upcastBool :: BS.CExpr -> BS.CExpr
    upcastBool e = withTypeAnnotation toTy $
                   BS.CApply
                     (BS.CVar BS.idUnpack)
                     [BS.CApply extendExpr [BS.CApply (BS.CVar BS.idPack) [e]]]

    -- upcast :: (BitExtend m n x) => x m -> x n
    -- upcast e = (extend e) :: ty
    --
    -- Convert a narrower integral type to a wider integral type (e.g.,
    -- `UInt 8` to `UInt 64` or `Int 8` to `Int 64`). Note that the `extend`
    -- operation is smart enough to choose between sign-extension and
    -- zero-extension depending on whether `x m` (i.e., fromTy) is a signed
    -- or unsigned type, respectively.
    upcast :: BS.CExpr -> BS.CExpr
    upcast e = withTypeAnnotation toTy $ BS.CApply extendExpr [e]

    -- downcast :: (BitExtend m n x) => x n -> x m
    -- downcast e = (truncate e) :: ty
    --
    -- Convert a wider integral type to a narrow integral type (e.g.,
    -- `UInt 64` to `UInt 8` or `Int 64` to `Int 8`) by truncating the most
    -- significant bits.
    downcast :: BS.CExpr -> BS.CExpr
    downcast e = withTypeAnnotation toTy $ BS.CApply truncateExpr [e]

    -- Apply upcast followed by unpackPack. This requires supplying the type to
    -- upcast to for type disambiguation purposes.
    unpackPackUpcast :: Type a -> BS.CExpr -> BS.CExpr
    unpackPackUpcast upcastTy e = unpackPack $
      withTypeAnnotation upcastTy $ BS.CApply extendExpr [e]

    -- castIntegralToFloatingPoint :: (FixedFloatCVT fromTy toTy) => fromTy toTy
    -- castIntegralToFloatingPoint e =
    --   ((vFixedToFloat e (0 :: UInt 64) Rnd_Nearest_Even).fst) :: tfl
    --
    -- While FloatingPoint does have a Bits instance, we don't want to convert
    -- an integral type to a FloatingPoint type using the Bits class, as the
    -- bit representation of an integral value differs quite a bit from the bit
    -- representation of a FloatingPoint value. Instead, we use the
    -- special-purpose FixedFloatCVT class for this task.
    castIntegralToFloatingPoint :: BS.CExpr -> BS.CExpr
    castIntegralToFloatingPoint e =
      withTypeAnnotation toTy $
      BS.CSelect
        (BS.CApply
          (BS.CVar (BS.mkId BS.NoPos "vFixedToFloat"))
          [ e
          , constNumTy Word64 0
          , fpRM
          ])
        BS.idPrimFst

    extendExpr   = BS.CVar $ BS.mkId BS.NoPos "extend"
    truncateExpr = BS.CVar $ BS.mkId BS.NoPos "truncate"

-- | Transform a Copilot Core literal, based on its value and type, into a
-- Bluespec expression.
constTy :: Type a -> a -> BS.CExpr
constTy ty =
  case ty of
    -- The treatment of scalar types is relatively straightforward. Note that
    -- Bool is an enum, so we must construct it using a CCon rather than with
    -- a CLit.
    Bool      -> \v -> BS.CCon (if v then BS.idTrue else BS.idFalse) []
    Int8      -> constInt ty . toInteger
    Int16     -> constInt ty . toInteger
    Int32     -> constInt ty . toInteger
    Int64     -> constInt ty . toInteger
    Word8     -> constInt ty . toInteger
    Word16    -> constInt ty . toInteger
    Word32    -> constInt ty . toInteger
    Word64    -> constInt ty . toInteger
    Float     -> constFP ty . realToFrac
    Double    -> constFP ty

    -- Translating a Copilot array literal to a Bluespec Vector is somewhat
    -- involved. Given a Copilot array {x_0, ..., x_(n-1)}, the
    -- corresponding Bluespec Vector will look something like:
    --
    --   let arr = update (... (update newVector 0 x_0)) (n-1) x_(n-1)
    --
    -- We use the `update` function instead of the := syntax (e.g.,
    -- { array_temp[0] := x; array_temp[1] := y; ...}) so that we can construct
    -- a Vector in a pure context.
    Array ty' -> constVector ty' . arrayElems

    -- Converting a Copilot struct { field_0 = x_0, ..., field_(n-1) = x_(n-1) }
    -- to a Bluespec struct is quite straightforward, given Bluespec's struct
    -- initialization syntax.
    Struct s -> \v ->
      BS.CStruct
        (Just True)
        (BS.mkId BS.NoPos $ fromString $ uppercaseName $ typeName s)
        (map (\(Value ty'' field@(Field val)) ->
               ( BS.mkId BS.NoPos $ fromString
                                  $ lowercaseName
                                  $ fieldName field
               , constTy ty'' val
               ))
             (toValues v))

-- | Transform a list of Copilot Core expressions of a given 'Type' into a
-- Bluespec @Vector@ expression.
constVector :: Type a -> [a] -> BS.CExpr
constVector ty = genVector (\_ -> constTy ty)

-- | Transform a list of Copilot Core expressions into a Bluespec @Vector@
-- expression. When invoking @genVector f es@, where @es@ has length @n@, the
-- resulting @Vector@ will consist of
-- @[f 0 (es !! 0), f 1 (es !! 1), ..., f (n-1) (es !! (n-1))]@.
genVector :: (Int -> a -> BS.CExpr) -> [a] -> BS.CExpr
genVector f vec =
  snd $
  foldl'
    (\(!i, !v) x ->
      ( i+1
      , cUpdateVector
          v
          (cLit (BS.LInt (BS.ilDec (toInteger i))))
          (f i x)
      ))
    (0, BS.CVar (BS.mkId BS.NoPos "newVector"))
    vec

-- | Translate a literal number of type @ty@ into a Bluespec expression.
--
-- PRE: The type of PRE is numeric (integer or floating-point), that
-- is, not boolean, struct or array.
constNumTy :: Type a -> Integer -> BS.CExpr
constNumTy ty =
  case ty of
    Float  -> constFP ty . fromInteger
    Double -> constFP ty . fromInteger
    _      -> constInt ty

-- | Translate a Copilot integer literal into a Bluespec expression.
constInt :: Type a -> Integer -> BS.CExpr
constInt ty i
    -- Bluespec intentionally does not support negative literal syntax (e.g.,
    -- -42), so we must create negative integer literals using the `negate`
    -- function.
    | i >= 0    = withTypeAnnotation ty $ cLit $ BS.LInt $ BS.ilDec i
    | otherwise = withTypeAnnotation ty $
                  BS.CApply
                    (BS.CVar $ BS.idNegateAt BS.NoPos)
                    [cLit $ BS.LInt $ BS.ilDec $ negate i]

-- | Translate a Copilot floating-point literal into a Bluespec expression.
constFP :: Type ty -> Double -> BS.CExpr
constFP ty d
    -- Bluespec intentionally does not support negative literal syntax (e.g.,
    -- -42.5), so we must create negative floating-point literals using the
    -- `negate` function.
    | d >= 0    = withTypeAnnotation ty $ cLit $ BS.LReal d
    | otherwise = withTypeAnnotation ty $
                  BS.CApply
                    (BS.CVar $ BS.idNegateAt BS.NoPos)
                    [cLit $ BS.LReal $ negate d]

-- | Create a Bluespec expression consisting of a literal.
cLit :: BS.Literal -> BS.CExpr
cLit = BS.CLit . BS.CLiteral BS.NoPos

-- | Create a Bluespec expression that indexes into a @Vector@.
cIndexVector :: BS.CExpr -> BS.CExpr -> BS.CExpr
cIndexVector vec idx =
  BS.CApply (BS.CVar (BS.mkId BS.NoPos "select")) [vec, idx]

-- | Create a Bluespec expression that updates a @Vector@ element at a
-- particular index.
cUpdateVector :: BS.CExpr -> BS.CExpr -> BS.CExpr -> BS.CExpr
cUpdateVector vec idx newElem =
  BS.CApply
    (BS.CVar (BS.mkId BS.NoPos "update"))
    [vec, idx, newElem]

-- | Create a Bluespec identifier for a floating-point function that Bluespec
-- imports using BDPI.
fpFunId :: Type a -> String -> BS.Id
fpFunId ty funPrefix =
    BS.mkId BS.NoPos $ fromString $ "bs_fp_" ++ funName
  where
    funName :: String
    funName =
      case ty of
        Float  -> funPrefix ++ "f"
        Double -> funPrefix
        _      -> impossible "fpFunId" "copilot-bluespec"

-- | Explicitly annotate an expression with a type signature. This is necessary
-- to prevent expressions from having ambiguous types in certain situations.
withTypeAnnotation :: Type a -> BS.CExpr -> BS.CExpr
withTypeAnnotation ty e = e `BS.CHasType` BS.CQType [] (transType ty)

-- | True if the type given is a floating point number.
typeIsFloating :: Type a -> Bool
typeIsFloating Float  = True
typeIsFloating Double = True
typeIsFloating _      = False

-- | We assume round-near-even throughout, but this variable can be changed if
-- needed. This matches the behavior of @fpRM@ in @copilot-theorem@'s
-- @Copilot.Theorem.What4.Translate@ module.
fpRM :: BS.CExpr
fpRM = BS.CCon (BS.mkId BS.NoPos "Rnd_Nearest_Even") []
