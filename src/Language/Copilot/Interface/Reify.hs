-- |

module Language.Copilot.Interface.Reify
  ( reify
  ) where

import Control.Monad (liftM, liftM2, liftM3)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.IORef
import Language.Copilot.Core ( Spec (..), WithSpec (..)
                             , CanonStream (..), Streamable )
import qualified Language.Copilot.Core as Core
import Language.Copilot.Interface.Stream (Stream (..))
import Language.Copilot.Interface.DynMap (DynMap, DynKey (..))
import qualified Language.Copilot.Interface.DynMap as D
import Language.Copilot.Interface.DynStableName

reify
  :: Streamable a
  => Stream a
  -> IO (WithSpec a)
reify e0 =
  do
    refCount   <- newIORef 0
    refVisited <- newIORef M.empty
    refDynMap  <- newIORef D.empty
    e <- dfs refCount refVisited refDynMap e0
    m <- readIORef refDynMap
    return $ WithSpec $ \ f -> f (Spec m e)

dfs
  :: Streamable a
  => IORef Int
  -> IORef (IntMap [(StableName, Int)])
  -> IORef (DynMap (CanonStream DynKey))
  -> Stream a
  -> IO (Core.Expr DynKey a)
dfs refCount refVisited refDynMap e0 =
  case e0 of
    Append _ _      -> liftM (Core.Drop 0)
                         (canonStream refCount refVisited refDynMap e0)
    Const x         -> return (Core.Const x)
    Drop k e1       -> case e1 of
                         Append _ _ -> liftM (Core.Drop k)
                            (canonStream refCount refVisited refDynMap e1)
                         _            ->
                            error "dfs: Drop" -- !!! This needs to be fixed !!!
    Extern cs       -> return (Core.Extern cs)
    Fun1 f e        -> liftM (Core.Fun1 f)
                         (dfs refCount refVisited refDynMap e)
    Fun2 f e1 e2    -> liftM2 (Core.Fun2 f)
                         (dfs refCount refVisited refDynMap e1)
                         (dfs refCount refVisited refDynMap e2)
    Fun3 f e1 e2 e3 -> liftM3 (Core.Fun3 f)
                         (dfs refCount refVisited refDynMap e1)
                         (dfs refCount refVisited refDynMap e2)
                         (dfs refCount refVisited refDynMap e3)

{-# INLINE canonStream #-}
canonStream
  :: Streamable a
  => IORef Int
  -> IORef (IntMap [(StableName, Int)])
  -> IORef (DynMap (CanonStream DynKey))
  -> Stream a
  -> IO (DynKey a)
canonStream refCount refVisited refDynMap e0 =
  do
    stn <- makeStableName e0
    let Append buf e = e0 -- avoids warning
    mk <- haveVisited stn refVisited
    case mk of
      Just k  -> return (DynKey k)
      Nothing -> addToVisited refCount refVisited refDynMap stn buf e

{-# INLINE haveVisited #-}
haveVisited
  :: StableName
  -> IORef (IntMap [(StableName, Int)])
  -> IO (Maybe Int)
haveVisited stn refVisited =
  do
    tab <- readIORef refVisited
    return (M.lookup (hashStableName stn) tab >>= lookup stn)

{-# INLINE addToVisited #-}
addToVisited
  :: Streamable a
  => IORef Int
  -> IORef (IntMap [(StableName, Int)])
  -> IORef (DynMap (CanonStream DynKey))
  -> StableName
  -> [a]
  -> Stream a
  -> IO (DynKey a)
addToVisited refCount refVisited refDynMap stn buf e0 =
  do
    k <- atomicModifyIORef refCount $ \ n -> (succ n, n)
    modifyIORef refVisited $ M.insertWith (++) (hashStableName stn) [(stn, k)]
    e <- dfs refCount refVisited refDynMap e0
    modifyIORef refDynMap $ D.insert k (CanonStream (DynKey k) buf e)
    return $ DynKey k
