-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Copilot.Interface.Reify
  ( reify
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.IORef
  (IORef, newIORef, readIORef, writeIORef, modifyIORef, atomicModifyIORef)
import Data.Type.Equality
import Language.Copilot.Core
import Language.Copilot.Interface.Stream (Stream (Stream))
import Prelude hiding (lookup)
import qualified Prelude as P
import System.Mem.StableName (StableName, makeStableName, hashStableName)
import Unsafe.Coerce (unsafeCoerce)

data Dyn :: (* -> *) -> * where
  Dyn :: Typed a => f a -> Dyn f

toDyn :: Typed a => f a -> Dyn f
toDyn = Dyn

fromDyn :: Typed a => Dyn f -> Maybe (f a)
fromDyn (Dyn x) =
  -- Proof at runtime that type 'a' is equal to type 'b'
  eqT typeOf__ typeOf__ >>= \ w -> Just (coerce (cong w) x)

dynMap :: (forall a . f a -> g a) -> Dyn f -> Dyn g
dynMap f (Dyn x) = Dyn (f x)

data Spec_ a = Spec_ (Map_ (Node Key_)) (Key_ a)

instance Specification Spec_ where
  runSpec (Spec_ m k) f = f m k

data Map_ f = Map_ (IntMap (Dyn f))

data Key_ a = Key_ Int

instance Rank2HeteroMap Map_ where
  type Key Map_ = Key_

  hlookup (Key_ k) (Map_ m) = x
    where
      Just x = M.lookup k m >>= fromDyn

  hmap f (Map_ m) = Map_ (M.map (dynMap f) m)

insert :: Typed a => Int -> f a -> Map_ f -> Map_ f
insert k x (Map_ m) = Map_ (M.insert k (toDyn x) m)

newtype DynStableName = DynStableName (StableName ())

instance Eq DynStableName where
  (DynStableName sn1) == (DynStableName sn2) = sn1 == sn2

makeDynStableName :: a -> IO DynStableName
makeDynStableName a = do
  sn <- makeStableName a
  return $ DynStableName (unsafeCoerce sn)

hashDynStableName :: DynStableName -> Int
hashDynStableName (DynStableName sn) = hashStableName sn

reify
  :: Streamable a
  => Stream a
  -> IO (Spec_ a)
reify (Stream e) =
  do
    refCount <- newIORef 1
    refTable <- newIORef M.empty
    refMap <- newIORef . Map_ $ M.empty
    k <- dfs refCount refTable refMap e
    m <- readIORef refMap
    return $ Spec_ m k

dfs
  :: Streamable a
  => IORef Int
  -> IORef (IntMap [(DynStableName, Int)])
  -> IORef (Map_ (Node Key_))
  -> Mu2 Node a
  -> IO (Key_ a)
dfs refCount refTable refMap (In x) =
  do
    stn <- makeDynStableName x
    tab <- readIORef refTable
    case M.lookup (hashDynStableName stn) tab >>= P.lookup stn of
      -- we have viseted the node before:
      Just k ->
        do
          writeIORef refTable tab
          return (Key_ k)
      -- we haven't viseted the the node before:
      Nothing ->
        do
          k <- atomicModifyIORef refCount $ \ n -> (succ n, n)
          writeIORef refTable $
            M.insertWith (++) (hashDynStableName stn) [(stn, k)] tab
          y <- traverse2 (dfs refCount refTable refMap) x
          modifyIORef refMap $ insert k y
          return $ Key_ k
