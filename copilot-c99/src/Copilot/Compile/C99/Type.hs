{-# LANGUAGE GADTs #-}

-- | Translate Copilot Core expressions and operators to C99.
module Copilot.Compile.C99.Type
    ( transType
    , transLocalVarDeclType
    , transTypeName
    )
  where

-- External imports
import qualified Language.C99.Simple as C

-- Internal imports: Copilot
import Copilot.Core ( Type (..), typeLength, typename )

-- | Translate a Copilot type to a C99 type.
transType :: Type a -> C.Type
transType ty = case ty of
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
  Array ty' -> C.Array (transType ty') len
    where
      len = Just $ C.LitInt $ fromIntegral $ typeLength ty
  Struct s  -> C.TypeSpec $ C.Struct (typename s)

-- | Translate a Copilot type to a valid (local) variable declaration C99 type.
--
-- If the type denotes an array, translate it to a pointer to whatever the
-- array holds. This special case is needed when the type is used for a local
-- variable declaration. We treat global variables differently (we generate
-- list initializers).
transLocalVarDeclType :: Type a -> C.Type
transLocalVarDeclType (Array ty') = C.Ptr $ transType ty'
transLocalVarDeclType ty          = transType ty

-- | Translate a Copilot type intro a C typename
transTypeName :: Type a -> C.TypeName
transTypeName ty = C.TypeName $ transType ty
