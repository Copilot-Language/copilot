-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Copilot.Core.Type
  ( Type (..)
  , Typed (..)
  ) where

import Control.Monad (mzero)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Type.Equality (EqT (eqT), (:=:) (Refl))
import Data.Word (Word8, Word16, Word32, Word64)
import Language.Copilot.Core.Array (Array)

data Type :: * -> * where
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
  Array  :: Typed t => Type t -> Type (Array t)

deriving instance Eq (Type a)

deriving instance Show (Type a)

class Typed a where
  typeOf   :: a -> Type a
  typeOf__ :: Type a
  typeOf _ = typeOf__
  typeOf__ = typeOf (undefined :: a)

instance Typed Bool   where typeOf__ = Bool
instance Typed Int8   where typeOf__ = Int8
instance Typed Int16  where typeOf__ = Int16
instance Typed Int32  where typeOf__ = Int32
instance Typed Int64  where typeOf__ = Int64
instance Typed Word8  where typeOf__ = Word8
instance Typed Word16 where typeOf__ = Word16
instance Typed Word32 where typeOf__ = Word32
instance Typed Word64 where typeOf__ = Word64
instance Typed Float  where typeOf__ = Float
instance Typed Double where typeOf__ = Double
instance Typed t => Typed (Array t) where typeOf__ = Array typeOf__

instance EqT Type where
  eqT Bool   Bool   = return Refl
  eqT Int8   Int8   = return Refl
  eqT Int16  Int16  = return Refl
  eqT Int32  Int32  = return Refl
  eqT Int64  Int64  = return Refl
  eqT Word8  Word8  = return Refl
  eqT Word16 Word16 = return Refl
  eqT Word32 Word32 = return Refl
  eqT Word64 Word64 = return Refl
  eqT Float  Float  = return Refl
  eqT Double Double = return Refl
  eqT (Array t1) (Array t2) =
    do
      Refl <- eqT t1 t2
      return Refl
  eqT _          _  = mzero
