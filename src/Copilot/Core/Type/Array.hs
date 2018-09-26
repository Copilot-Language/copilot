--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

-- | Implementation of an array that uses type literals to store length. No
-- explicit indexing is used for the input data. Supports arbitrary nesting of
-- arrays.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Copilot.Core.Type.Array
  ( Array
  , array
  , flatten
  , size
  , Flatten
  , InnerType
  ) where

import GHC.TypeLits     (Nat, KnownNat, natVal)
import Data.Proxy       (Proxy (..))

data Array (n :: Nat) t where
  Array :: [t] -> Array n t

instance Show t => Show (Array n t) where
  show (Array xs) = show xs

array :: forall n t. KnownNat n => [t] -> Array n t
array xs | datalen == typelen = Array xs
         | otherwise          = error errmsg where
  datalen = length xs
  typelen = fromIntegral $ natVal (Proxy :: Proxy n)
  errmsg = "Length of data (" ++ show datalen ++
           ") does not match length of type (" ++ show typelen ++ ")."


type family InnerType x where
  InnerType (Array _ x) = InnerType x
  InnerType x           = x


class Flatten a b where
  flatten :: Array n a -> [b]

instance Flatten a a where
  flatten (Array xs) = xs

instance Flatten a b => Flatten (Array n a) b where
  flatten (Array xss) = concat $ map flatten xss

instance Foldable (Array n) where
  foldr f base (Array xs) = foldr f base xs


size :: forall a n b. (Flatten a b, b ~ InnerType a) => Array n a -> Int
size xs = length $ (flatten xs :: [b])
