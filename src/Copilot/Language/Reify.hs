--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE Rank2Types #-}

module Copilot.Language.Reify
  ( reify
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.IORef
import Copilot.Core (Typed, Id, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Spec (Spec, runSpec, Trigger (..), TriggerArg (..))
import Copilot.Language.Stream (Stream (..))
import Copilot.Language.Reify.DynStableName

--------------------------------------------------------------------------------

newtype WrapExpr a = WrapExpr
  { unWrapExpr :: forall e . Core.Expr e => e a }

wrapExpr :: (forall e . Core.Expr e => e a) -> WrapExpr a
wrapExpr = WrapExpr

--------------------------------------------------------------------------------

reify :: Spec () -> IO Core.Spec
reify spec =
  do
    let triggers = runSpec spec
    refCount   <- newIORef 0
    refVisited <- newIORef M.empty
    refMap     <- newIORef []
    xs <- mapM (mkTrigger refCount refVisited refMap) triggers
    ys <- readIORef refMap
    return $ Core.Spec (reverse ys) xs

--------------------------------------------------------------------------------

{-# INLINE mkTrigger #-}
mkTrigger
  :: IORef Int
  -> IORef (IntMap [(StableName, Int)])
  -> IORef [Core.Stream]
  -> Trigger
  -> IO Core.Trigger
mkTrigger refCount refVisited refMap (Trigger name guard args) =
  do
    w1 <- mkExpr refCount refVisited refMap guard
    args' <- mapM mkTriggerArg args
    return $ Core.Trigger name (unWrapExpr w1) args'

  where

  mkTriggerArg :: TriggerArg -> IO Core.TriggerArg
  mkTriggerArg (TriggerArg e) =
    do
      w <- mkExpr refCount refVisited refMap e
      return $ Core.TriggerArg (unWrapExpr w) typeOf

--------------------------------------------------------------------------------

mkExpr
  :: Typed a
  => IORef Int
  -> IORef (IntMap [(StableName, Int)])
  -> IORef [Core.Stream]
  -> Stream a
  -> IO (WrapExpr a)
mkExpr refCount refVisited refMap e0 =
  case e0 of
    Append _ _ _    -> do s <- mkStream refCount refVisited refMap e0
                          return $ wrapExpr $ Core.drop typeOf 0 s
    Const x         -> return $ wrapExpr $ Core.const typeOf x
    Drop k e1       -> case e1 of
                         Append _ _ _ ->
                           do
                             s <- mkStream refCount refVisited refMap e1
                             return $ wrapExpr $ Core.drop typeOf k s
                         _ -> error "dfs: Drop" -- !!! This needs to be fixed !!!
    Extern cs       -> return $ wrapExpr $ Core.extern typeOf cs
    Op1 op e        -> do
                         w <- mkExpr refCount refVisited refMap e
                         return $ wrapExpr $ Core.op1 op (unWrapExpr w)
    Op2 op e1 e2    -> do
                         w1 <- mkExpr refCount refVisited refMap e1
                         w2 <- mkExpr refCount refVisited refMap e2
                         return $ wrapExpr $ Core.op2 op
                           (unWrapExpr w1) (unWrapExpr w2)
    Op3 op e1 e2 e3 -> do
                         w1 <- mkExpr refCount refVisited refMap e1
                         w2 <- mkExpr refCount refVisited refMap e2
                         w3 <- mkExpr refCount refVisited refMap e3
                         return $ wrapExpr $ Core.op3 op
                           (unWrapExpr w1) (unWrapExpr w2) (unWrapExpr w3)

--------------------------------------------------------------------------------

{-# INLINE mkStream #-}
mkStream
  :: Typed a
  => IORef Int
  -> IORef (IntMap [(StableName, Int)])
  -> IORef [Core.Stream]
  -> Stream a
  -> IO Id
mkStream refCount refVisited refMap e0 =
  do
    stn <- makeStableName e0
    let Append buf _ e = e0 -- avoids warning
    mk <- haveVisited stn
    case mk of
      Just id_ -> return id_
      Nothing  -> addToVisited stn buf e

  where

  {-# INLINE haveVisited #-}
  haveVisited :: StableName -> IO (Maybe Int)
  haveVisited stn =
    do
      tab <- readIORef refVisited
      return (M.lookup (hashStableName stn) tab >>= lookup stn)

  {-# INLINE addToVisited #-}
  addToVisited
    :: Typed a
    => StableName
    -> [a]
    -> Stream a
    -> IO Id
  addToVisited stn buf e =
    do
      id_ <- atomicModifyIORef refCount $ \ n -> (succ n, n)
      modifyIORef refVisited $
        M.insertWith (++) (hashStableName stn) [(stn, id_)]
      w <- mkExpr refCount refVisited refMap e
      modifyIORef refMap $
        (:) (Core.Stream id_ buf Nothing (unWrapExpr w) typeOf)
      return id_

--------------------------------------------------------------------------------