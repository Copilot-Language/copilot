-- |

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}

module Copilot.Language.Reify
  ( reify
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.IORef
import Copilot.Core (Spec (..), Streamable, Id)
import qualified Copilot.Core as Core
import Copilot.Language.Stream (Stream (..), Trigger (..))
import Copilot.Language.Reify.DynStableName

newtype WrapExpr α = WrapExpr
  { unWrapExpr ∷ forall η . Core.Expr η => η α }

reify ∷ [Trigger] → IO Spec
reify triggers =
  do
    refCount   <- newIORef 0
    refVisited <- newIORef M.empty
    refMap     <- newIORef []
    xs <- sequence $ map (mkTrigger refCount refVisited refMap) triggers
    ys <- readIORef refMap
    return $ Spec (reverse ys) xs

{-# INLINE mkTrigger #-}
mkTrigger
  ∷ IORef Int
  → IORef (IntMap [(StableName, Int)])
  → IORef [Core.Stream]
  → Trigger
  → IO Core.Trigger
mkTrigger refCount refVisited refMap (Trigger name guard e) =
  do
    w1 <- mkExpr refCount refVisited refMap guard
    w2 <- mkExpr refCount refVisited refMap e
    return $ Core.Trigger name (unWrapExpr w1) (unWrapExpr w2)

mkExpr
  ∷ Streamable α
  ⇒ IORef Int
  → IORef (IntMap [(StableName, Int)])
  → IORef [Core.Stream]
  → Stream α
  → IO (WrapExpr α)
mkExpr refCount refVisited refMap e0 =
  case e0 of
    Append _ _ _    → do s <- mkStream refCount refVisited refMap e0
                         return $ WrapExpr $ Core.drop 0 s
    Const x         → return $ WrapExpr $ Core.const x
    Drop k e1       → case e1 of
                        Append _ _ _ →
                          do
                            s <- mkStream refCount refVisited refMap e1
                            return $ WrapExpr $ Core.drop k s
                        _ → error "dfs: Drop" -- !!! This needs to be fixed !!!
    Extern cs       → return $ WrapExpr $ Core.extern cs
    Op1 op e        → do
                        w <- mkExpr refCount refVisited refMap e
                        return $ WrapExpr $ Core.op1 op (unWrapExpr w)
    Op2 op e1 e2    → do
                        w1 <- mkExpr refCount refVisited refMap e1
                        w2 <- mkExpr refCount refVisited refMap e2
                        return $ WrapExpr $ Core.op2 op
                          (unWrapExpr w1) (unWrapExpr w2)
    Op3 op e1 e2 e3 → do
                        w1 <- mkExpr refCount refVisited refMap e1
                        w2 <- mkExpr refCount refVisited refMap e2
                        w3 <- mkExpr refCount refVisited refMap e3
                        return $ WrapExpr $ Core.op3 op
                          (unWrapExpr w1) (unWrapExpr w2) (unWrapExpr w3)

{-# INLINE mkStream #-}
mkStream
--  ∷ (Streamable α, Core.Expr η)
  ∷ Streamable α
  ⇒ IORef Int
  → IORef (IntMap [(StableName, Int)])
  → IORef [Core.Stream]
  → Stream α
  → IO Id
mkStream refCount refVisited refMap e0 =
  do
    stn <- makeStableName e0
    let Append buf _ e = e0 -- avoids warning
    mk <- haveVisited stn refVisited
    case mk of
      Just id_ → return id_
      Nothing  → addToVisited refCount refVisited refMap stn buf e

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
  → IORef [Core.Stream]
  → StableName
  → [α]
  → Stream α
  → IO Id
addToVisited refCount refVisited refMap stn buf e0 =
  do
    id_ <- atomicModifyIORef refCount $ \ n → (succ n, n)
    modifyIORef refVisited $
      M.insertWith (++) (hashStableName stn) [(stn, id_)]
    w <- mkExpr refCount refVisited refMap e0
    modifyIORef refMap $
      (:) (Core.Stream id_ buf Nothing (unWrapExpr w))
    return id_
