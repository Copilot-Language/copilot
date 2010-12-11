-- | Safe casting of values.

module Language.Copilot.Language.Casting ( cast ) where

import qualified Language.Atom as A

import Data.Int
import Data.Word

import Language.Copilot.Core

-- | Cast 'a' into 'b'.  We only allow "safe casts" to larger types.
class Streamable a => Castable a where
  castFrom :: (Streamable b, A.IntegralE b) => Spec a -> Spec b
  cast :: (Castable b, Streamable b) => Spec b -> Spec a

castErr :: String -> A.Type -> String
castErr toT fromT = "Error: cannot cast type " ++ show fromT 
                    ++ " into " ++ toT ++ ".  Only casts guarnateed \n" 
                    ++ "not to change sign and to larger types are allowed."

instance Castable Bool where
  castFrom = F (\b -> if b then 1 else 0)
               (\b -> A.mux b 1 0)
  cast x =  error $ castErr "Bool" (getAtomType x)

instance Castable Word8 where
  castFrom = F (fromInteger . toInteger) 
               (A.Retype . A.ue)
  cast x =  case getAtomType x of
    A.Bool   -> castFrom x
    t        -> error $ castErr "Word8" t

instance Castable Word16 where
  castFrom = F (fromInteger . toInteger) 
               (A.Retype . A.ue)
  cast x =  case getAtomType x of
    A.Bool   -> castFrom x
    A.Word8  -> castFrom x
    t        -> error $ castErr "Word16" t

instance Castable Word32 where
  castFrom = F (fromInteger . toInteger) 
               (A.Retype . A.ue)
  cast x =  case getAtomType x of
    A.Bool   -> castFrom x
    A.Word8  -> castFrom x
    A.Word16 -> castFrom x
    t        -> error $ castErr "Word32" t

instance Castable Word64 where
  castFrom = F (fromInteger . toInteger) 
               (A.Retype . A.ue)
  cast x =   case getAtomType x of
    A.Bool   -> castFrom x
    A.Word8  -> castFrom x
    A.Word16 -> castFrom x
    A.Word32 -> castFrom x
    t        -> error $ castErr "Word64" t

instance Castable Int8 where
  castFrom = F (fromInteger . toInteger) 
               (A.Retype . A.ue)
  cast x =  case getAtomType x of
    A.Bool   -> castFrom x
    t        -> error $ castErr "Int8" t

instance Castable Int16 where
  castFrom = F (fromInteger . toInteger) 
               (A.Retype . A.ue)
  cast x = case getAtomType x of
    A.Bool   -> castFrom x
    A.Int8   -> castFrom x
    A.Word8  -> castFrom x
    t        -> error $ castErr "Int16" t

instance Castable Int32 where
  castFrom = F (fromInteger . toInteger) 
               (A.Retype . A.ue)
  cast x =  case getAtomType x of
    A.Bool   -> castFrom x
    A.Int8  -> castFrom x
    A.Int16 -> castFrom x
    A.Word8  -> castFrom x
    A.Word16  -> castFrom x
    t        -> error $ castErr "Int32" t

instance Castable Int64 where
  castFrom = F (fromInteger . toInteger) 
               (A.Retype . A.ue)
  cast x =  case getAtomType x of
    A.Bool   -> castFrom x
    A.Int8  -> castFrom x
    A.Int16 -> castFrom x
    A.Int32 -> castFrom x
    A.Word8  -> castFrom x
    A.Word16  -> castFrom x
    A.Word32  -> castFrom x
    t        -> error $ castErr "Int64" t
