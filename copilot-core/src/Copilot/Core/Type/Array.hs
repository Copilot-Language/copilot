{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Safe                #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

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
    , arrayUpdate
    )
  where

-- External imports
import Data.Proxy   (Proxy (..))
import GHC.TypeLits (KnownNat, Nat, natVal, type(-))

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

-- | Update element of array to given element.
--
-- PRE: the second argument denotes a valid index in the array.
arrayUpdate :: Array n a -> Int -> a -> Array n a
arrayUpdate (Array []) _ _ = error errMsg
  where
    errMsg = "copilot-core: arrayUpdate: Attempt to update empty array"

arrayUpdate (Array (_:xs)) 0 y = Array (y:xs)

arrayUpdate (Array (x:xs)) n y =
    arrayAppend x (arrayUpdate (Array xs) (n - 1) y)
  where
    -- | Append to an array while preserving length information at the type
    -- level.
    arrayAppend :: a -> Array (n - 1) a -> Array n a
    arrayAppend x' (Array xs') = Array (x':xs')
