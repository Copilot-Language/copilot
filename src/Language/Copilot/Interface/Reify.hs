-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- |

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Interface.Reify
  ( reify
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.IORef
import Language.Copilot.Core (Spec (..), Strm (..), Trig (..), Streamable, Expr__, lit)
import qualified Language.Copilot.Core as Core
import qualified Language.Copilot.Core.Dynamic.Map as D
import qualified Language.Copilot.Core.HeteroMap as H
import Language.Copilot.Interface.Stream (Stream (..))
import Language.Copilot.Interface.Reify.DynStableName

newtype WrapExpr α = WrapExpr { unWrapExpr ∷ Expr__ α }

reify
  ∷ Streamable α
  ⇒ Stream α
  → IO Spec
reify e0 =
  do
    refCount   <- newIORef 0
    refVisited <- newIORef M.empty
    refMap     <- newIORef D.empty
    w <- dfs refCount refVisited refMap e0
    m <- readIORef refMap
    return $ Spec m [Trig "out" (lit False) (unWrapExpr w)]

dfs
  ∷ (Streamable α)
  ⇒ IORef Int
  → IORef (IntMap [(StableName, Int)])
  → IORef (D.Map Strm)
  → Stream α
  → IO (WrapExpr α)
dfs refCount refVisited refMap e0 =
  case e0 of
    Append _ _      → do s <- canonStream refCount refVisited refMap e0
                         return $ WrapExpr $ Core.drp 0 s
    Const x         → return $ WrapExpr $ Core.lit x
    Drop k e1       → case e1 of
                        Append _ _ →
                          do
                            s <- canonStream refCount refVisited refMap e1
                            return $ WrapExpr $ Core.drp k s
                        _ → error "dfs: Drop" -- !!! This needs to be fixed !!!
    Extern cs       → return $ WrapExpr $ Core.ext cs
    Op1 op e        → do
                        w <- dfs refCount refVisited refMap e
                        return $ WrapExpr $ Core.op1 op (unWrapExpr w)
    Op2 op e1 e2    → do
                        w1 <- dfs refCount refVisited refMap e1
                        w2 <- dfs refCount refVisited refMap e2
                        return $ WrapExpr $ Core.op2 op
                          (unWrapExpr w1) (unWrapExpr w2)
    Op3 op e1 e2 e3 → do
                        w1 <- dfs refCount refVisited refMap e1
                        w2 <- dfs refCount refVisited refMap e2
                        w3 <- dfs refCount refVisited refMap e3
                        return $ WrapExpr $ Core.op3 op
                          (unWrapExpr w1) (unWrapExpr w2) (unWrapExpr w3)

{-# INLINE canonStream #-}
canonStream
--  ∷ (Streamable α, Core.Expr η)
  ∷ Streamable α
  ⇒ IORef Int
  → IORef (IntMap [(StableName, Int)])
  → IORef (D.Map (Strm))
  → Stream α
  → IO H.Key
canonStream refCount refVisited refMap e0 =
  do
    stn <- makeStableName e0
    let Append buf e = e0 -- avoids warning
    mk <- haveVisited stn refVisited
    case mk of
      Just k  → return (H.Key k)
      Nothing → addToVisited refCount refVisited refMap stn buf e

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
--  ∷ (Streamable α, Core.Expr η)
  ∷ Streamable α
  ⇒ IORef Int
  → IORef (IntMap [(StableName, Int)])
  → IORef (D.Map (Strm))
  → StableName
  → [α]
  → Stream α
  → IO H.Key
addToVisited refCount refVisited refMap stn buf e0 =
  do
    k <- atomicModifyIORef refCount $ \ n → (succ n, n)
    modifyIORef refVisited $ M.insertWith (++) (hashStableName stn) [(stn, k)]
    w <- dfs refCount refVisited refMap e0
    modifyIORef refMap $ D.insert k (Strm buf (unWrapExpr w))
    return (H.Key k)
