-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}

module Language.Copilot.Interface.Reify
  ( reify
  , Map2
  , Key
  , lookup
  , fmap2
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.IORef
  (IORef, newIORef, readIORef, writeIORef, modifyIORef, atomicModifyIORef)
import Language.Copilot.Core.Node hiding (fmap2)
import Language.Copilot.Core.Streamable (Streamable)
import Prelude hiding (lookup)
import qualified Prelude as P
import System.Mem.StableName (StableName, makeStableName, hashStableName)
import Unsafe.Coerce (unsafeCoerce)

-- !!! Map2 needs it own module with a proper interface !!!
data Map2 :: * -> (* -> *) -> * where
  Map2 :: Streamable a => IntMap (f a) -> Map2 x f

newtype Key x a = Key Int

newtype DynStableName = DynStableName (StableName ())

instance Eq DynStableName where
  (DynStableName sn1) == (DynStableName sn2) = sn1 == sn2

makeDynStableName :: a -> IO DynStableName
makeDynStableName a = do
  sn <- makeStableName a
  return $ DynStableName (unsafeCoerce sn)

hashDynStableName :: DynStableName -> Int
hashDynStableName (DynStableName sn) = hashStableName sn

lookup
  :: forall a f x . Streamable a
  => Key x a
  -> Map2 x f
  -> f a
lookup (Key k) (Map2 m) =
  case M.lookup k m of
    Nothing -> error "Reify.lookup: M.lookup = Nothing"
    Just x  -> unsafeCoerce x

insert
  :: Streamable a
  => Key x a
  -> f a
  -> Map2 x f
  -> Map2 x f
insert (Key k) x (Map2 m) =
  Map2 $ M.insert k (unsafeCoerce x) m

fmap2
  :: (forall a . f a -> g a)
  -> Map2 x f
  -> Map2 x g
fmap2 f (Map2 m) = Map2 (M.map f m)

reify
  :: Streamable a
  => Mu2 Node a
  -> IO (Map2 x (Node (Key x)), Key x a)
reify x =
  do
    refCount <- newIORef 1
    refTable <- newIORef M.empty
    refMap <- newIORef . Map2 $ M.singleton 0 (Const False)
    k <- dfs refCount refTable refMap x
    m <- readIORef refMap
    return (m, k)

dfs
  :: Streamable a
  => IORef Int
  -> IORef (IntMap [(DynStableName, Int)])
  -> IORef (Map2 x (Node (Key x)))
  -> Mu2 Node a
  -> IO (Key x a)
dfs refCount refTable refMap (In x) =
  do
    stn <- makeDynStableName x
    tab <- readIORef refTable
    case M.lookup (hashDynStableName stn) tab >>= P.lookup stn of
      -- we have viseted the node before:
      Just k ->
        do
          writeIORef refTable tab
          return (Key k)
      -- we haven't viseted the the node before:
      Nothing ->
        do
          k <- atomicModifyIORef refCount $ \ n -> (succ n, n)
          writeIORef refTable $
            M.insertWith (++) (hashDynStableName stn) [(stn, k)] tab
          y <- traverse2 (dfs refCount refTable refMap) x
          modifyIORef refMap $ insert (Key k) y
          return $ Key k
