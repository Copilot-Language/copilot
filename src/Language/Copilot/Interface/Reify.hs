-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- |

{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Interface.Reify
  ( reify
  ) where

import Control.Monad (liftM, liftM2, liftM3)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.IORef
import Language.Copilot.Core (Spec (..), WithSpec (..), Strm (..), Streamable)
import qualified Language.Copilot.Core as Core
import Language.Copilot.Interface.Stream (Stream (..))
import Language.Copilot.Interface.DynMap (DynMap, DynKey (..))
import qualified Language.Copilot.Interface.DynMap as D
import Language.Copilot.Interface.DynStableName

reify
  ∷ Streamable α
  ⇒ Stream α
  → IO (WithSpec α)
reify e0 =
  do
    refCount   <- newIORef 0
    refVisited <- newIORef M.empty
    refDynMap  <- newIORef D.empty
    e <- dfs refCount refVisited refDynMap e0
    m <- readIORef refDynMap
    return $ WithSpec $ \ f → f (Spec m e)

dfs
  ∷ Streamable α
  ⇒ IORef Int
  → IORef (IntMap [(StableName, Int)])
  → IORef (DynMap (Strm DynKey))
  → Stream α
  → IO (Core.Expr DynKey α)
dfs refCount refVisited refDynMap e0 =
  case e0 of
    Append _ _     → liftM (Core.Drop 0)
                       (canonStream refCount refVisited refDynMap e0)
    Const x        → return (Core.Const x)
    Drop k e1      → case e1 of
                       Append _ _ → liftM (Core.Drop k)
                         (canonStream refCount refVisited refDynMap e1)
                       _ → error "dfs: Drop" -- !!! This needs to be fixed !!!
    Extern cs      → return (Core.Extern cs)
    Op1 f e        → liftM (Core.Op1 f)
                       (dfs refCount refVisited refDynMap e)
    Op2 f e1 e2    → liftM2 (Core.Op2 f)
                       (dfs refCount refVisited refDynMap e1)
                       (dfs refCount refVisited refDynMap e2)
    Op3 f e1 e2 e3 → liftM3 (Core.Op3 f)
                       (dfs refCount refVisited refDynMap e1)
                       (dfs refCount refVisited refDynMap e2)
                       (dfs refCount refVisited refDynMap e3)

{-# INLINE canonStream #-}
canonStream
  ∷ Streamable α
  ⇒ IORef Int
  → IORef (IntMap [(StableName, Int)])
  → IORef (DynMap (Strm DynKey))
  → Stream α
  → IO (DynKey α)
canonStream refCount refVisited refDynMap e0 =
  do
    stn <- makeStableName e0
    let Append buf e = e0 -- avoids warning
    mk <- haveVisited stn refVisited
    case mk of
      Just k  → return (DynKey k)
      Nothing → addToVisited refCount refVisited refDynMap stn buf e

{-# INLINE haveVisited #-}
haveVisited
  ∷ StableName
  → IORef (IntMap [(StableName, Int)])
  → IO (Maybe Int)
haveVisited stn refVisited =
  do
    tab <- readIORef refVisited
    return (M.lookup (hashStableName stn) tab >>= lookup stn)

{-# INLINE addToVisited #-}
addToVisited
  ∷ Streamable α
  ⇒ IORef Int
  → IORef (IntMap [(StableName, Int)])
  → IORef (DynMap (Strm DynKey))
  → StableName
  → [α]
  → Stream α
  → IO (DynKey α)
addToVisited refCount refVisited refDynMap stn buf e0 =
  do
    k <- atomicModifyIORef refCount $ \ n → (succ n, n)
    modifyIORef refVisited $ M.insertWith (++) (hashStableName stn) [(stn, k)]
    e <- dfs refCount refVisited refDynMap e0
    modifyIORef refDynMap $ D.insert k (Strm Core.typeOf buf e)
    return $ DynKey k
