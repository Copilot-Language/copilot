--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Transforms a Copilot Language specification into a Copilot Core
-- specification.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Language.Reify
  ( reify
  ) where

import Copilot.Core (Typed, Type, Id, typeOf)
import qualified Copilot.Core as Core
--import Copilot.Language.Reify.Sharing (makeSharingExplicit)
import Copilot.Language.Analyze (analyze)
import Copilot.Language.Spec
import Copilot.Language.Stream (Stream (..))
import Data.IORef
import Prelude hiding (id)
import System.Mem.StableName.Dynamic
import System.Mem.StableName.Dynamic.Map (Map)
import qualified System.Mem.StableName.Dynamic.Map as M

--------------------------------------------------------------------------------

newtype WrapExpr a = WrapExpr
  { unWrapExpr :: forall e . Core.Expr e => e a }

--------------------------------------------------------------------------------

reify :: Spec -> IO Core.Spec
reify spec =
  do
    analyze spec
    let trigs = triggers  $ runSpec spec
    let obsvs = observers $ runSpec spec
    refMkId      <- newIORef 0
    refVisited    <- newIORef M.empty
    refMap        <- newIORef []
    coreTriggers  <- mapM (mkTrigger  refMkId refVisited refMap) trigs
    coreObservers <- mapM (mkObserver refMkId refVisited refMap) obsvs
    coreStreams   <- readIORef refMap
    return $
      Core.Spec
        { Core.specStreams   = reverse coreStreams
        , Core.specObservers = coreObservers
        , Core.specTriggers  = coreTriggers }

--------------------------------------------------------------------------------

{-# INLINE mkObserver #-}
mkObserver
  :: IORef Int
  -> IORef (Map Core.Id)
  -> IORef [Core.Stream]
  -> Observer
  -> IO Core.Observer
mkObserver refMkId refStreams refMap (Observer name e) =
  do 
    w <- mkExpr refMkId refStreams refMap e
    return $
      Core.Observer
         { Core.observerName     = name
         , Core.observerExpr     = unWrapExpr w
         , Core.observerExprType = typeOf }

--------------------------------------------------------------------------------

{-# INLINE mkTrigger #-}
mkTrigger
  :: IORef Int
  -> IORef (Map Core.Id)
  -> IORef [Core.Stream]
  -> Trigger
  -> IO Core.Trigger
mkTrigger refMkId refStreams refMap (Trigger name guard args) =
  do
    w1 <- mkExpr refMkId refStreams refMap guard 
    args' <- mapM mkTriggerArg args
    return $
      Core.Trigger
        { Core.triggerName  = name
        , Core.triggerGuard = unWrapExpr w1 
        , Core.triggerArgs  = args' }

  where

  mkTriggerArg :: TriggerArg -> IO Core.TriggerArg
  mkTriggerArg (TriggerArg e) =
    do
      w <- mkExpr refMkId refStreams refMap e
      return $ Core.TriggerArg (unWrapExpr w) typeOf

--------------------------------------------------------------------------------

{-# INLINE mkExpr #-}
mkExpr
  :: Typed a
  => IORef Int
  -> IORef (Map Core.Id)
  -> IORef [Core.Stream]
  -> Stream a
  -> IO (WrapExpr a)
mkExpr refMkId refStreams refMap = go

--  (>>= go) . makeSharingExplicit refMkId

  where

  go
    :: Typed a
    => Stream a
    -> IO (WrapExpr a)
  go e0 =
    case e0 of
      Append _ _ _    -> do s <- mkStream refMkId refStreams refMap e0
                            return $ WrapExpr $ Core.drop typeOf 0 s
      Drop k e1       -> case e1 of
                           Append _ _ _ ->
                             do
                               s <- mkStream refMkId refStreams refMap e1
                               return $ WrapExpr $ Core.drop typeOf (fromIntegral k) s
                           _ -> error "dfs: Drop" -- !!! Fix this !!!
      Const x         -> return $ WrapExpr $ Core.const typeOf x
      Local e f       -> do
                           id <- mkId refMkId
                           let cs = "local_" ++ show id
                           w1 <- go e
                           w2 <- go (f (Var cs))
                           return $ WrapExpr $ Core.local typeOf typeOf cs
                             (unWrapExpr w1) (unWrapExpr w2)
      Var cs          -> return $ WrapExpr $ Core.var typeOf cs
      Extern cs       -> return $ WrapExpr $ Core.extern typeOf cs
      Op1 op e        -> do
                           w <- go e
                           return $ WrapExpr $ Core.op1 op (unWrapExpr w)
      Op2 op e1 e2    -> do
                           w1 <- go e1
                           w2 <- go e2
                           return $ WrapExpr $ Core.op2 op
                             (unWrapExpr w1) (unWrapExpr w2)
      Op3 op e1 e2 e3 -> do
                           w1 <- go e1
                           w2 <- go e2
                           w3 <- go e3
                           return $ WrapExpr $ Core.op3 op
                             (unWrapExpr w1) (unWrapExpr w2) (unWrapExpr w3)

--------------------------------------------------------------------------------

{-# INLINE mkStream #-}
mkStream
  :: Typed a
  => IORef Int
  -> IORef (Map Core.Id)
  -> IORef [Core.Stream]
  -> Stream a
  -> IO Id
mkStream refMkId refStreams refMap e0 =
  do
    dstn <- makeDynStableName e0
    let Append buf _ e = e0 -- avoids warning
    mk <- haveVisited dstn
    case mk of
      Just id_ -> return id_
      Nothing  -> addToVisited dstn buf e

  where

  {-# INLINE haveVisited #-}
  haveVisited :: DynStableName -> IO (Maybe Int)
  haveVisited dstn =
    do
      tab <- readIORef refStreams
      return (M.lookup dstn tab)

  {-# INLINE addToVisited #-}
  addToVisited
    :: Typed a
    => DynStableName
    -> [a]
    -> Stream a
    -> IO Id
  addToVisited dstn buf e =
    do
      id <- mkId refMkId
      modifyIORef refStreams (M.insert dstn id)
      w <- mkExpr refMkId refStreams refMap e
      modifyIORef refMap $ (:)
        Core.Stream
          { Core.streamId         = id
          , Core.streamBuffer     = buf
          , Core.streamGuard      = Core.const (typeOf :: Type Bool) True
          , Core.streamExpr       = unWrapExpr w
          , Core.streamExprType   = typeOf }
      return id

--------------------------------------------------------------------------------

mkId :: IORef Int -> IO Id
mkId refMkId = atomicModifyIORef refMkId $ \ n -> (succ n, n)

--------------------------------------------------------------------------------
