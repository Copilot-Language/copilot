-- |

-- !! THIS MODULE NEADS FURTHER CLEANING !!

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Copilot.Interface.Reify
  ( Spec
  , reify
  ) where

import Data.Array
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.IORef
  (IORef, newIORef, readIORef, writeIORef, modifyIORef, atomicModifyIORef)
import Data.Type.Equality
import Language.Copilot.Core hiding (Array)
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

instance Functor2 Dyn where
  fmap2 f (Dyn x) = Dyn (f x)

data Map_ f = Map_ (Array Int (Dyn f))

newtype Key_ a = Key_ Int
  deriving (Eq, Ord, Show)

instance Functor2 Map_ where
  fmap2 f (Map_ m) = Map_ (fmap2 f `fmap` m)

instance HeteroMap Map_ where
  type Key Map_ = Key_

  hlookup (Key_ k) (Map_ m) = x
    where
      Just x = fromDyn (m ! k)

  key2Int (Key_ k) = k

data Spec a = Spec (Map_ (Node Key_)) (Key_ a)

instance Specification Spec where
  runSpec (Spec m k) f = f m k

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
  -> IO (Spec a)
reify (Stream e) =
  do
    refCount <- newIORef 1
    refTable <- newIORef M.empty
    refMap <- newIORef M.empty
    k <- dfs refCount refTable refMap e
    m <- readIORef refMap
    let arr = array (0, M.size m) (M.toList m)
    return $ Spec (Map_ arr) k

dfs
  :: Streamable a
  => IORef Int
  -> IORef (IntMap [(DynStableName, Int)])
  -> IORef (IntMap (Dyn (Node Key_)))
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
          modifyIORef refMap $ M.insert k (toDyn y)
          return $ Key_ k
