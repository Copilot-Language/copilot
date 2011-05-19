-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Copilot.Interface.DynMap
  ( DynMap
  , DynKey (..)
  , empty
  , insert
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Type.Equality
import Language.Copilot.Core (HasType (..))
import Language.Copilot.Core.HeteroMap (HeteroMap (..), Key, Key2Int (..))

data Dyn :: (* -> *) -> * where
  Dyn
    :: HasType a
    => f a
    -> Dyn f

newtype DynMap f = DynMap (IntMap (Dyn f))

newtype DynKey a = DynKey Int
  deriving (Eq, Ord, Show)

instance HeteroMap DynMap where
  type Key DynMap = DynKey

  lookup (DynKey k) (DynMap m) = x
    where
      Just x = M.lookup k m >>= fromDyn

  mapWithKey f (DynMap m) =
    DynMap $
      M.mapWithKey (\ k x -> mapDyn (f (DynKey k)) x) m

  foldMapWithKey = undefined

  traverseWithKey _ _ = undefined

instance Key2Int DynKey where
  key2int (DynKey n) = n

empty
  :: DynMap f
empty = DynMap M.empty

insert
  :: HasType a
  => Int
  -> f a
  -> DynMap f
  -> DynMap f
insert k x (DynMap m) = DynMap $ M.insert k (toDyn x) m

toDyn
  :: HasType a
  => f a
  -> Dyn f
toDyn = Dyn

fromDyn
  :: HasType a
  => Dyn f
  -> Maybe (f a)
fromDyn (Dyn x) =
  -- Proof at runtime that type 'a' is equal to type 'b'
  eqT typeOf typeOf >>= \ w -> Just (coerce (cong w) x)

mapDyn
  :: (forall a . f a -> g a)
  -> Dyn f
  -> Dyn g
mapDyn f (Dyn x) = Dyn (f x)
