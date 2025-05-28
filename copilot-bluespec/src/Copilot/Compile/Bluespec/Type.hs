{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Translate Copilot Core expressions and operators to Bluespec.
module Copilot.Compile.Bluespec.Type
  ( transType
  , tVector
  ) where

-- External imports
import Data.String (IsString (..))
import qualified Language.Bluespec.Classic.AST as BS
import qualified Language.Bluespec.Classic.AST.Builtin.Ids as BS
import qualified Language.Bluespec.Classic.AST.Builtin.Types as BS

-- Internal imports: Copilot
import Copilot.Core

-- Internal imports
import Copilot.Compile.Bluespec.Name

-- | Translate a Copilot type to a Bluespec type.
transType :: Type a -> BS.CType
transType ty = case ty of
  Bool   -> BS.tBool
  Int8   -> BS.tInt  `BS.TAp` BS.cTNum  8 BS.NoPos
  Int16  -> BS.tInt  `BS.TAp` BS.cTNum 16 BS.NoPos
  Int32  -> BS.tInt  `BS.TAp` BS.cTNum 32 BS.NoPos
  Int64  -> BS.tInt  `BS.TAp` BS.cTNum 64 BS.NoPos
  Word8  -> BS.tUInt `BS.TAp` BS.cTNum  8 BS.NoPos
  Word16 -> BS.tUInt `BS.TAp` BS.cTNum 16 BS.NoPos
  Word32 -> BS.tUInt `BS.TAp` BS.cTNum 32 BS.NoPos
  Word64 -> BS.tUInt `BS.TAp` BS.cTNum 64 BS.NoPos

  Float -> BS.TCon $
    BS.TyCon
      { BS.tcon_name = BS.mkId BS.NoPos "Float"
      , BS.tcon_kind = Just BS.KStar
      , BS.tcon_sort = BS.TItype 0 $ tFloatingPoint `BS.TAp`
                                     BS.cTNum  8 BS.NoPos `BS.TAp`
                                     BS.cTNum 23 BS.NoPos
      }
  Double -> BS.TCon $
    BS.TyCon
      { BS.tcon_name = BS.mkId BS.NoPos "Double"
      , BS.tcon_kind = Just BS.KStar
      , BS.tcon_sort = BS.TItype 0 $ tFloatingPoint `BS.TAp`
                                     BS.cTNum 11 BS.NoPos `BS.TAp`
                                     BS.cTNum 52 BS.NoPos
      }
  Array ty' -> tVector `BS.TAp` BS.cTNum len BS.NoPos `BS.TAp` transType ty'
    where
      len = toInteger $ typeLength ty
  Struct s -> BS.TCon $
    BS.TyCon
      { BS.tcon_name = BS.mkId BS.NoPos $
                       fromString $
                       uppercaseName $
                       typeName s
      , BS.tcon_kind = Just BS.KStar
      , BS.tcon_sort =
          BS.TIstruct BS.SStruct $
          map (\(Value _tu field) ->
                BS.mkId BS.NoPos $
                fromString $
                lowercaseName $
                fieldName field)
              (toValues s)
      }

-- | The @Vector@ Bluespec data type.
tVector :: BS.CType
tVector = BS.TCon $
  BS.TyCon
    { BS.tcon_name = BS.idVector
    , BS.tcon_kind = Just (BS.Kfun BS.KNum (BS.Kfun BS.KStar BS.KStar))
    , BS.tcon_sort =
        BS.TIdata
          { BS.tidata_cons = [BS.mkId BS.NoPos "V"]
          , BS.tidata_enum = False
          }
    }

-- | The @FloatingPoint@ Bluespec struct type.
tFloatingPoint :: BS.CType
tFloatingPoint = BS.TCon $
  BS.TyCon
    { BS.tcon_name = BS.mkId BS.NoPos "FloatingPoint"
    , BS.tcon_kind = Just (BS.Kfun BS.KNum (BS.Kfun BS.KNum BS.KStar))
    , BS.tcon_sort =
        BS.TIstruct BS.SStruct [ BS.mkId BS.NoPos "sign"
                               , BS.mkId BS.NoPos "exp"
                               , BS.mkId BS.NoPos "sfd"
                               ]
    }
