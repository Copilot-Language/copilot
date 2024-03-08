{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- |
-- Copyright: (c) 2011 National Institute of Aerospace / Galois, Inc.
--
-- Implementation of an array that uses type literals to store length. No
-- explicit indexing is used for the input data. Supports arbitrary nesting of
-- arrays.
module Copilot.Core.Type.Array
    ( Array
    , array
    , arrayElems
    )
  where

-- External imports
import Data.Proxy   (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal)

-- | Implementation of an array that uses type literals to store length.
data Array (n :: Nat) t where
  Array :: [t] -> Array n t

instance Show t => Show (Array n t) where
  show (Array xs) = show xs

-- | Smart array constructor that only type checks if the length of the given
-- list matches the length of the array at type level.
array :: forall n t. KnownNat n => [t] -> Array n t
array xs | datalen == typelen = Array xs
         | otherwise          = error errmsg
  where
    datalen = length xs
    typelen = fromIntegral $ natVal (Proxy :: Proxy n)
    errmsg = "Length of data (" ++ show datalen ++
             ") does not match length of type (" ++ show typelen ++ ")."

-- | Return the elements of an array.
arrayElems :: Array n a -> [a]
arrayElems (Array xs) = xs
