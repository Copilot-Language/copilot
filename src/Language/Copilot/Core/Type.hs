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
import qualified Language.Copilot.Core.Array as A
import Unsafe.Coerce (unsafeCoerce)

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
  Array
    :: Typed t
    => Type t
    -> Type (A.Array t)
  Maybe
    :: Typed t
    => Type t
    -> Type (Maybe t)
  Tup2
    :: (Typed t1, Typed t2)
    => Type t1 -> Type t2
    -> Type (t1, t2)
  Tup3
    :: (Typed t1, Typed t2, Typed t3)
    => Type t1 -> Type t2 -> Type t3
    -> Type (t1, t2, t3)
  Tup4
    :: (Typed t1, Typed t2, Typed t3, Typed t4)
    => Type t1 -> Type t2 -> Type t3 -> Type t4
    -> Type (t1, t2, t3, t4)

deriving instance Eq (Type a)

deriving instance Show (Type a)

-- A bit hacky, but short...
instance EqT Type where
  eqT t1 t2 =
    if t1 == unsafeCoerce t2
      then return (unsafeCoerce Refl)
      else mzero

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

instance Typed t => Typed (A.Array t) where
  typeOf__ = Array typeOf__

instance Typed t => Typed (Maybe t) where
  typeOf__ = Maybe typeOf__

instance (Typed t1, Typed t2) => Typed (t1, t2) where
  typeOf__ = Tup2 typeOf__ typeOf__

instance (Typed t1, Typed t2, Typed t3) => Typed (t1, t2, t3) where
  typeOf__ = Tup3 typeOf__ typeOf__ typeOf__

instance (Typed t1, Typed t2, Typed t3, Typed t4)
  => Typed (t1, t2, t3, t4) where
    typeOf__ = Tup4 typeOf__ typeOf__ typeOf__ typeOf__

{-
instance EqT Type where
  eqT BoolT   BoolT   = return Refl
  eqT Int8T   Int8T   = return Refl
  eqT Int16T  Int16T  = return Refl
  eqT Int32T  Int32T  = return Refl
  eqT Int64T  Int64T  = return Refl
  eqT Word8T  Word8T  = return Refl
  eqT Word16T Word16T = return Refl
  eqT Word32T Word32T = return Refl
  eqT Word64T Word64T = return Refl
  eqT FloatT  FloatT  = return Refl
  eqT DoubleT DoubleT = return Refl
  eqT (ArrayT t1) (ArrayT t2) =
    do
      Refl <- eqT t1 t2
      return Refl
  eqT (MaybeT t1) (MaybeT t2) =
    do
      Refl <- eqT t1 t2
      return Refl
  eqT _ _ = mzero
-}
