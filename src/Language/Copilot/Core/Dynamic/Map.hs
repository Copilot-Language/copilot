-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- |

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Language.Copilot.Core.Dynamic.Map
  ( Map
  , Key
  , empty
  , insert
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
--import qualified Data.Traversable as T
import Language.Copilot.Core.Type
import Language.Copilot.Core.Type.Equality
import Language.Copilot.Core.Dynamic
import qualified Language.Copilot.Core.HeteroMap as H

newtype Map f = Map (IntMap (DynamicF f))

type Key = Int

instance H.Map Map where
  lookup (H.Key k) (Map m) = M.lookup k m >>= fromDynamicF

  map f (Map m) =
      Map $ M.map g m
    where
     g dyn = mapDyn f dyn

  mapWithKey f (Map m) =
      Map $ M.mapWithKey g m
    where
      g k dyn = mapDyn (f (H.Key k)) dyn

  fold f b0 (Map m) =
      M.fold g b0 m
    where
      g dyn b = accDyn f dyn b

  foldWithKey f b0 (Map m) =
      M.foldWithKey g b0 m
    where
      g k dyn b = accDyn (f (H.Key k)) dyn b
{-
  traverse f (Map m) =
      Map `fmap` T.traverse g m
    where
      g dyn = appDyn f dyn

  traverseWithKey f (DynMap m) =
      (DynMap . M.fromList) `fmap` T.traverse g (M.toList m)
    where
      g (k, dyn) = (,) k `fmap` (appDyn (f (DynKey k)) dyn)
-}

empty :: Map f
empty = Map M.empty

insert
  :: Typed α
  => Int
  -> f α
  -> Map f
  -> Map f
insert k x (Map m) = Map $ M.insert k (toDynamicF x) m

mapDyn
  :: (forall α . f α -> g α)
  -> DynamicF f
  -> DynamicF g
mapDyn f (DynamicF x t) = DynamicF (f x) t

accDyn
  :: (forall α . f α -> β -> β)
  -> (DynamicF f -> β -> β)
accDyn f (DynamicF x _) b = f x b

appDyn
  :: Functor t
  => (forall α . f α -> t (g α))
  -> (DynamicF f -> t (DynamicF g))
appDyn f (DynamicF x t) = (`DynamicF` t) `fmap` (f x)
