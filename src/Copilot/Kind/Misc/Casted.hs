--------------------------------------------------------------------------------

module Copilot.Kind.Misc.Casted
  ( Dyn
  , toDyn
  , Casted
  , isCastable
  , extractI
  , extractB
  ) where

import Copilot.Core
import Copilot.Core.Type.Equality
import Copilot.Core.Type.Dynamic

--------------------------------------------------------------------------------

type Dyn = Dynamic Type

class Casted b where
  cast :: Dyn -> Maybe b

--------------------------------------------------------------------------------

instance Casted Integer where

  cast (Dynamic v tv)
    | Just Refl <- tv =~= Int8    = Just $ toInteger v 
    | Just Refl <- tv =~= Int16   = Just $ toInteger v
    | Just Refl <- tv =~= Int32   = Just $ toInteger v
    | Just Refl <- tv =~= Int64   = Just $ toInteger v
    | Just Refl <- tv =~= Word16  = Just $ toInteger v
    | Just Refl <- tv =~= Word8   = Just $ toInteger v
    | Just Refl <- tv =~= Word32  = Just $ toInteger v
    | Just Refl <- tv =~= Word64  = Just $ toInteger v
    | otherwise                   = Nothing
 

instance Casted Bool where

  cast (Dynamic v tv)
    | Just Refl <- tv =~= Bool  = Just v
    | otherwise                 = Nothing

--------------------------------------------------------------------------------

isCastable :: forall a b . (Casted b) => Type a -> Type b -> Bool
isCastable ta _
  | Just (_ :: b) <- cast $ toDyn ta (uninitialized ta) = True
  | otherwise = False
                  
--------------------------------------------------------------------------------

badCastMsg :: String
badCastMsg = "Bad type cast"

extractI :: Dyn -> Integer
extractI v
  | Just (vi :: Integer) <- cast v = vi
  | otherwise = error badCastMsg
    
extractB :: Dyn -> Bool
extractB v
  | Just (vb :: Bool) <- cast v = vb
  | otherwise = error badCastMsg

--------------------------------------------------------------------------------
