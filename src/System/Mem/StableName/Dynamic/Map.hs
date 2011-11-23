--------------------------------------------------------------------------------
-- Most of this code is taken from 'http://github.com/ekmett/stable-maps'.
--------------------------------------------------------------------------------

module System.Mem.StableName.Dynamic.Map
    ( Map(..)
    , empty
    , null
    , singleton
    , member
    , notMember
    , insert
    , insertWith
    , insertWith'
    , lookup
    , find
    , findWithDefault
    ) where

import qualified Prelude
import Prelude hiding (lookup, null)
import System.Mem.StableName.Dynamic
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

import Copilot.Core.Error (impossible)

newtype Map a = Map { getMap :: IntMap [(DynStableName, a)] } 

empty :: Map a
empty = Map IntMap.empty

null :: Map a -> Bool
null (Map m) = IntMap.null m

singleton :: DynStableName -> a -> Map a
singleton k v = Map $ IntMap.singleton (hashDynStableName k) [(k,v)]

member :: DynStableName -> Map a -> Bool
member k m = case lookup k m of
    Nothing -> False
    Just _ -> True

notMember :: DynStableName -> Map a -> Bool
notMember k m = not $ member k m 

insert :: DynStableName -> a -> Map a -> Map a
insert k v = Map . IntMap.insertWith (++) (hashDynStableName k) [(k,v)] . getMap

-- | /O(log n)/. Insert with a function for combining the new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if the key does not exist
-- in the map. If the key does exist, the function will insert the pair
-- @(key, f new_value old_value)@
insertWith :: (a -> a -> a) -> DynStableName -> a -> Map a -> Map a
insertWith f k v = Map . IntMap.insertWith go (hashDynStableName k) [(k,v)] . getMap 
    where 
        go _ ((k',v'):kvs) 
            | k == k' = (k', f v v') : kvs
            | otherwise = (k',v') : go undefined kvs
        go _ [] = []

-- | Same as 'insertWith', but with the combining function applied strictly.
insertWith' :: (a -> a -> a) -> DynStableName -> a -> Map a -> Map a
insertWith' f k v = Map . IntMap.insertWith go (hashDynStableName k) [(k,v)] . getMap 
    where 
        go _ ((k',v'):kvs) 
            | k == k' = let v'' = f v v' in v'' `seq` (k', v'') : kvs
            | otherwise = (k', v') : go undefined kvs
        go _ [] = []

-- | /O(log n)/. Lookup the value at a key in the map.
-- 
-- The function will return the corresponding value as a @('Just' value)@
-- or 'Nothing' if the key isn't in the map.
lookup :: DynStableName -> Map v -> Maybe v
lookup k (Map m) = do
    pairs <- IntMap.lookup (hashDynStableName k) m
    Prelude.lookup k pairs

find :: DynStableName -> Map v -> v
find k m = case lookup k m of
    Nothing -> impossible "find" "copilot-language"
    Just x -> x 

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns the default value @def@
-- when the key is not in the map.
findWithDefault :: v -> DynStableName -> Map v -> v
findWithDefault dflt k m = maybe dflt id $ lookup k m 
