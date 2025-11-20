{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE Trustworthy               #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
-- |
-- Description: Typing for Core.
-- Copyright:   (c) 2011 National Institute of Aerospace / Galois, Inc.
--
-- All expressions and streams in Core are accompanied by a representation of
-- the types of the underlying expressions used or carried by the streams.
-- This information is needed by the compiler to generate code, since it must
-- initialize variables and equivalent representations for those types in
-- the target languages.
module Copilot.Core.Type
    ( Type (..)
    , Typed (..)
    , typeOfDefault
    , UType (..)
    , SimpleType (..)

    , typeSize
    , typeLength

    , Value (..)
    , toValues
    , toValuesDefault
    , Field (..)
    , typeName
    , typeNameDefault

    , Struct
    , fieldName
    , accessorName
    , updateField
    , updateFieldDefault
    )
  where

-- External imports
import           Data.Char               (isLower, isUpper, toLower)
import           Data.Coerce             (coerce)
import           Data.Int                (Int16, Int32, Int64, Int8)
import qualified Data.Kind               as DK
import           Data.List               (intercalate)
import           Data.Proxy              (Proxy (..))
import           Data.Type.Equality      as DE
import           Data.Typeable           (Typeable, eqT, typeRep)
import           Data.Word               (Word16, Word32, Word64, Word8)
import           GHC.Generics            (D1, Datatype (..), Generic (..),
                                          K1 (..), M1 (..), U1 (..), (:*:) (..))
import           GHC.TypeLits            (KnownNat, KnownSymbol, Symbol, natVal,
                                          sameNat, sameSymbol, symbolVal)

-- Internal imports
import           Copilot.Core.Type.Array (Array)

-- | The value of that is a product or struct, defined as a constructor with
-- several fields.
class Struct a where
  -- | Returns the name of struct in the target language.
  typeName :: a -> String

  -- | Transforms all the struct's fields into a list of values.
  toValues :: a -> [Value a]

  -- | Update the value of a struct field. This is only used by the Copilot
  -- interpreter.
  --
  -- If you do not plan to use the interpreter, you can omit an implementation
  -- of this method. If you do so, it is recommended that you derive a 'Generic'
  -- instance for the struct data type. This is because in a future release, the
  -- default implementation of 'updateField' (which will be picked if there is
  -- not a manually written implementation) will be changed to require a
  -- 'Generic' instance.
  --
  -- In order to implement 'updateField', pick one of the following approaches:
  --
  -- * Derive a 'Generic' instance for the struct data type and then define
  --   @'updateField' = 'updateFieldDefault'@ in the 'Struct' instance.
  --
  -- * Manually implement 'updateField' by doing the following for each 'Field'
  --   in a struct:
  --
  --   1. Check that the name of the 'Field' matches the name of the supplied
  --      'Value' (using 'GHC.TypeLits.sameSymbol').
  --
  --   2. Check that the type of the 'Field' matches the 'Type' of the supplied
  --      'Value' (using 'DE.testEquality').
  --
  --   3. If both (1) and (2) succeed, update the corresponding struct field
  --      using a record update.
  --
  --   For a complete end-to-end example that demonstrates how to manually
  --   implement 'updateField' and use it in the Copilot interpreter, see the
  --   @examples/StructsUpdateField.hs@ example in the @copilot@ library.
  updateField :: a -> Value t -> a
  updateField = error $ unlines
    [ "Field updates not supported for this type."
    , "(Perhaps you need to implement 'updateField' for a 'Struct' instance?)"
    ]

-- | The field of a struct, together with a representation of its type.
data Value a =
  forall s t . (Typeable t, KnownSymbol s, Show t) => Value (Type t) (Field s t)

-- | A field in a struct. The name of the field is a literal at the type
-- level.
data Field (s :: Symbol) t = Field t

-- | Extract the name of a field.
fieldName :: forall s t . KnownSymbol s => Field s t -> String
fieldName _ = symbolVal (Proxy :: Proxy s)

-- | Extract the name of an accessor (a function that returns a field of a
-- struct).
accessorName :: forall a s t . (Struct a, KnownSymbol s)
             => (a -> Field s t) -> String
accessorName _ = symbolVal (Proxy :: Proxy s)

instance (KnownSymbol s, Show t) => Show (Field s t) where
  show f@(Field v) = fieldName f ++ ":" ++ show v

instance {-# OVERLAPPABLE #-} (Typed t, Struct t) => Show t where
  show t = "<" ++ fields ++ ">"
    where
      fields = intercalate "," $ map showfield (toValues t)
      showfield (Value _ field) = show field

-- | A Type representing the types of expressions or values handled by
-- Copilot Core.
--
-- Note that both arrays and structs use dependently typed features. In the
-- former, the length of the array is part of the type. In the latter, the
-- names of the fields are part of the type.
data Type :: DK.Type -> DK.Type where
  Bool   :: Type Bool
  Int8   :: Type Int8
  Int16  :: Type Int16
  Int32  :: Type Int32
  Int64  :: Type Int64
  Word8  :: Type Word8
  Word16 :: Type Word16
  Word32 :: Type Word32
  Word64 :: Type Word64
  Float  :: Type Float
  Double :: Type Double
  Array  :: forall n t . ( KnownNat n
                         , Typed t
                         ) => Type t -> Type (Array n t)
  Struct :: (Typed a, Struct a) => a -> Type a
deriving instance Show (Type a)

-- | Return the length of an array from its type
typeLength :: forall n t . KnownNat n => Type (Array n t) -> Int
typeLength _ = fromIntegral $ natVal (Proxy :: Proxy n)

-- | Return the total (nested) size of an array from its type
typeSize :: forall n t . KnownNat n => Type (Array n t) -> Int
typeSize ty@(Array ty'@(Array _)) = typeLength ty * typeSize ty'
typeSize ty@(Array _            ) = typeLength ty
typeSize ty = error $ "There is a bug in the type checker " ++ show ty

instance TestEquality Type where
  testEquality Bool   Bool   = Just DE.Refl
  testEquality Int8   Int8   = Just DE.Refl
  testEquality Int16  Int16  = Just DE.Refl
  testEquality Int32  Int32  = Just DE.Refl
  testEquality Int64  Int64  = Just DE.Refl
  testEquality Word8  Word8  = Just DE.Refl
  testEquality Word16 Word16 = Just DE.Refl
  testEquality Word32 Word32 = Just DE.Refl
  testEquality Word64 Word64 = Just DE.Refl
  testEquality Float  Float  = Just DE.Refl
  testEquality Double Double = Just DE.Refl
  testEquality (Array t1) (Array t2) =
      testArrayEquality t1 t2
    where
      testArrayEquality :: forall n1 a1 n2 a2.
                           (KnownNat n1, KnownNat n2)
                        => Type a1
                        -> Type a2
                        -> Maybe (Array n1 a1 :~: Array n2 a2)
      testArrayEquality ty1 ty2
        | Just DE.Refl <- sameNat (Proxy :: Proxy n1) (Proxy :: Proxy n2)
        , Just DE.Refl <- testEquality ty1 ty2
        = Just DE.Refl
        | otherwise
        = Nothing
  testEquality (Struct _) (Struct _) = eqT
  testEquality _ _ = Nothing

-- | A simple, monomorphic representation of types that facilitates putting
-- variables in heterogeneous lists and environments in spite of their types
-- being different.
data SimpleType where
  SBool   :: SimpleType
  SInt8   :: SimpleType
  SInt16  :: SimpleType
  SInt32  :: SimpleType
  SInt64  :: SimpleType
  SWord8  :: SimpleType
  SWord16 :: SimpleType
  SWord32 :: SimpleType
  SWord64 :: SimpleType
  SFloat  :: SimpleType
  SDouble :: SimpleType
  SArray  :: Type t -> SimpleType
  SStruct :: SimpleType

-- | Type equality, used to help type inference.

-- This instance is necessary, otherwise the type of SArray can't be inferred.
instance Eq SimpleType where
  SBool   == SBool   = True
  SInt8   == SInt8   = True
  SInt16  == SInt16  = True
  SInt32  == SInt32  = True
  SInt64  == SInt64  = True
  SWord8  == SWord8  = True
  SWord16 == SWord16 = True
  SWord32 == SWord32 = True
  SWord64 == SWord64 = True
  SFloat  == SFloat  = True
  SDouble == SDouble = True
  (SArray t1) == (SArray t2) | Just DE.Refl <- testEquality t1 t2 = True
                             | otherwise                          = False
  SStruct == SStruct = True
  _ == _ = False

-- | A typed expression, from which we can obtain the two type representations
-- used by Copilot: 'Type' and 'SimpleType'.
class (Show a, Typeable a) => Typed a where
  typeOf     :: Type a
  simpleType :: Type a -> SimpleType
  simpleType _ = SStruct

instance Typed Bool where
  typeOf       = Bool
  simpleType _ = SBool

instance Typed Int8 where
  typeOf       = Int8
  simpleType _ = SInt8

instance Typed Int16 where
  typeOf       = Int16
  simpleType _ = SInt16

instance Typed Int32 where
  typeOf       = Int32
  simpleType _ = SInt32

instance Typed Int64 where
  typeOf       = Int64
  simpleType _ = SInt64

instance Typed Word8 where
  typeOf       = Word8
  simpleType _ = SWord8

instance Typed Word16 where
  typeOf       = Word16
  simpleType _ = SWord16

instance Typed Word32 where
  typeOf       = Word32
  simpleType _ = SWord32

instance Typed Word64 where
  typeOf       = Word64
  simpleType _ = SWord64

instance Typed Float where
  typeOf       = Float
  simpleType _ = SFloat

instance Typed Double where
  typeOf       = Double
  simpleType _ = SDouble

instance (Typeable t, Typed t, KnownNat n) => Typed (Array n t) where
  typeOf               = Array typeOf
  simpleType t         = case t of
    Array t' -> SArray t'
    o        -> error $ "There is a bug in the type checker " ++ show o

-- | A untyped type (no phantom type).
data UType = forall a . Typeable a => UType (Type a)

instance Eq UType where
  UType ty1 == UType ty2 = typeRep ty1 == typeRep ty2

-- * GHC.Generics-based defaults

-- | A default implementation of 'typeName' that leverages 'Generic'. In order
-- to use this, make sure you derive a 'Generic' instance for your data type and
-- then define @'typeName' = 'typeNameDefault'@ in its 'Struct' instance.
--
-- This generates a struct name that consists of the name of the original
-- Haskell data type, but converted to snake_case.
typeNameDefault :: (Generic a, GDatatype (Rep a)) => a -> String
typeNameDefault = convert . gTypeName . from
  where
    convert :: String -> String
    convert = convert' True True

    convert' :: Bool   -- ^ Is this the first letter
             -> Bool   -- ^ Was the previous letter capital
             -> String -- ^ Remainder of the string
             -> String
    convert' _ _ []    = []
    convert' _ v [x]
      | v && isUpper x = toLower x : []
      | isUpper x      = '_' : toLower x : []
      | otherwise      = x : []
    convert' b v (x1:x2:xs)
      | not b && isUpper x1 && (isLower x2 || not v)
      = '_' : toLower x1 : convert' False (isUpper x1) (x2:xs)
      | otherwise
      = toLower x1 : convert' False (isUpper x1) (x2:xs)

-- | A default implementation of 'toValues' that leverages 'Generic'. In order
-- to use this, make sure you derive a 'Generic' instance for your data type and
-- then define @'toValues' = 'toValuesDefault'@ in its 'Struct' instance.
toValuesDefault :: (Generic a, GStruct (Rep a)) => a -> [Value a]
toValuesDefault x = coerce $ gToValues $ from x

-- | A default implementation of 'updateField' that leverages 'Generic'. In
-- order to use this, make sure you derive a 'Generic' instance for your data
-- type and then define @'updateField' = 'updateFieldDefault'@ in its 'Struct'
-- instance.
updateFieldDefault :: (Generic a, GStruct (Rep a)) => a -> Value t -> a
updateFieldDefault a v@(Value _ field)
    | updated   = to a'
    | otherwise = error $ "Unexpected field: " ++ show field
  where
    (a', updated) = gUpdateField (from a) v

-- | A default implementation of 'typeOf' that leverages 'Generic'. In order to
-- use this, make sure you derive a 'Generic' instance for your data type and
-- then define @'typeOf' = 'typeOfDefault'@ in its 'Typed' instance.
typeOfDefault ::
  forall a. (Typed a, Struct a, Generic a, GTypedStruct (Rep a)) => Type a
typeOfDefault = Struct $ to $ gStructPlaceholder @(Rep a) @()

-- ** Generic-based classes (not exported)

-- | Capture the name of a Haskell data type from its 'Generic' metadata.
class GDatatype f where
  -- | Returns the name of a Haskell data type. (Note that this differs from
  -- 'typeName', which is expected to return the name of the struct in the
  -- /target/ language).
  gTypeName :: f p -> String

-- | The only 'GDatatype' instance we need is for 'D1', which describes
-- 'Datatype' metadata (@d@). We ignore all other metadata (@_f@).
instance Datatype d => GDatatype (D1 d _f) where
  gTypeName = datatypeName

-- | Perform struct-related operations on 'Generic' representation types.
class GStruct f where
  -- | Transforms all the struct representation's fields into a list of values.
  gToValues :: f p -> [Value (f p)]

  -- | Update the value of a struct representation's field. This returns two
  -- things:
  --
  -- 1. @f p@: The struct representation, but with the field updated.
  --
  -- 2. 'Bool': This is 'True' if the field was successfully updated and 'False'
  --    otherwise. If this returns 'False', it is the responsibility of the
  --    caller to raise an error.
  gUpdateField :: f p -> Value t -> (f p, Bool)

-- | 'U1' represents a data constructor with no fields. As such, 'gToValues'
-- returns an empty list of 'Value's, and 'gUpdateField' does not update
-- anything.
instance GStruct U1 where
  gToValues U1 = []
  gUpdateField u _ = (u, False)

-- | 'M1' is only used to store metadata, which the 'GStruct' class does not
-- make use of. As such, this instance discards the metadata and recurses.
instance GStruct f => GStruct (M1 _i _c f) where
  gToValues (M1 x) = coerce (gToValues x)
  gUpdateField (M1 x) v = (M1 x', updated)
    where
      (x', updated) = gUpdateField x v

-- | @(':*:')@ represents a data constructor with multiple fields.
instance (GStruct f, GStruct g) => GStruct (f :*: g) where
  -- Recursively compute two lists of Values and append them.
  gToValues (f :*: g) = coerce (gToValues f) ++ coerce (gToValues g)
  -- Recursively attempt to update the field in both branches and combine the
  -- updated branches. We will have successfully updated the field if either
  -- branch was successfully updated.
  gUpdateField (f :*: g) v = (f' :*: g', updatedF || updatedG)
    where
      (f', updatedF) = gUpdateField f v
      (g', updatedG) = gUpdateField g v

-- | 'K1' represents a single field in a data constructor. This is the base
-- case.
instance (KnownSymbol name, Typed ty, c ~ Field name ty) =>
    GStruct (K1 _i c) where
  -- Now that we have the field, return it in a singleton list.
  gToValues (K1 field) = [Value typeOf field]
  -- In order to update the field, we check that the field names and types
  -- match. If they do, return the updated field and declare the update as
  -- successful. Otherwise, return the old field and declare the update as
  -- unsuccessful.
  gUpdateField (K1 oldField) (Value newTy (newField :: Field newName newTy)) =
    case (sameSymbol pName pNewName, testEquality ty newTy) of
      (Just Refl, Just Refl) -> (K1 newField, True)
      _                      -> (K1 oldField, False)
    where
      pName    = Proxy @name
      pNewName = Proxy @newName
      ty       = typeOf @ty

-- | Compute a 'Generic' placeholder value to use for a struct type.
class GTypedStruct f where
  -- | A placeholder value to supply to use in a generic implementation of
  -- 'typeOf' for a struct type.
  gStructPlaceholder :: f p

-- | 'U1' represents a data constructor with no fields. As such,
-- 'gStructPlaceholder' simply returns the data constructor with no other
-- information.
instance GTypedStruct U1 where
  gStructPlaceholder = U1

-- | 'M1' is only used to store metadata, which the 'GTypedStruct' class does
-- not make use of. As such, this instance recursively computes a placeholder
-- value without inspecting the metadata.
instance GTypedStruct f => GTypedStruct (M1 _i _c f) where
  gStructPlaceholder = M1 gStructPlaceholder

-- | @(':*:')@ represents a data constructor with multiple fields. As such,
-- 'gStructPlaceholder' recursively computes placeholders for each field and
-- combines them into the overall data constructor.
instance (GTypedStruct f, GTypedStruct g) => GTypedStruct (f :*: g) where
  gStructPlaceholder = gStructPlaceholder :*: gStructPlaceholder

-- | 'K1' represents a single field in a data constructor. This is the base
-- case. This instance computes a placeholder value that works for any field of
-- any type.
instance (c ~ Field name ty) => GTypedStruct (K1 _i c) where
  -- We use 'undefined' as the actual value for the 'Field' because Copilot
  -- never inspects the value.
  gStructPlaceholder = K1 $ Field undefined
