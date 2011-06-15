--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Language.Reify
  ( reify
  ) where

import Copilot.Core (Typed, Id, typeOf)
import qualified Copilot.Core as Core
import Copilot.Language.Spec
  (Let (..), lets, triggers, Spec, runSpec, Trigger (..), TriggerArg (..))
import Copilot.Language.Stream (Stream (..))
import Data.IORef
import Prelude hiding (id)
import System.Mem.StableName.Dynamic
import System.Mem.StableName.Dynamic.Map (Map)
import qualified System.Mem.StableName.Dynamic.Map as M

--------------------------------------------------------------------------------

newtype WrapExpr a = WrapExpr
  { unWrapExpr :: forall e . Core.Expr e => e a }

{-
data Shared = forall a . Shared
  { sharedExpr :: forall e . Core.Expr e => e a
  , sharedType :: Core.Type a }
-}

--------------------------------------------------------------------------------

reify :: Spec -> IO Core.Spec
reify spec =
  do
    let trigs = triggers $ runSpec spec
    let letExprs = lets $ runSpec spec
    refCount     <- newIORef 0
    refVisited   <- newIORef M.empty
    refMap       <- newIORef []
    coreTriggers <- mapM (mkTrigger refCount refVisited refMap) trigs
    coreLets     <- mapM (mkLet refCount refVisited refMap) letExprs
    coreStreams  <- readIORef refMap
    return $
      Core.Spec
        { Core.specStreams   = reverse coreStreams
        , Core.specObservers = []
        , Core.specTriggers  = coreTriggers 
        , Core.specLets      = coreLets}

--------------------------------------------------------------------------------

{-# INLINE mkLet #-}
mkLet
  :: IORef Int
  -> IORef (Map Core.Id)
  -> IORef [Core.Stream]
  -> Let
  -> IO Core.Let
mkLet refCount refStreams refMap (Let var e) =
  do 
    w <- mkExpr refCount refStreams refMap e
    return $ Core.Let
               { Core.letVar  = var
               , Core.letExpr = unWrapExpr w
               , Core.letType = typeOf }

--------------------------------------------------------------------------------

{-# INLINE mkTrigger #-}
mkTrigger
  :: IORef Int
  -> IORef (Map Core.Id)
  -> IORef [Core.Stream]
  -> Trigger
  -> IO Core.Trigger
mkTrigger refCount refStreams refMap (Trigger name guard args) =
  do
    w1 <- mkExpr refCount refStreams refMap guard 
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
      w <- mkExpr refCount refStreams refMap e
      return $ Core.TriggerArg (unWrapExpr w) typeOf

--------------------------------------------------------------------------------

mkExpr
  :: Typed a
  => IORef Int
  -> IORef (Map Core.Id)
  -> IORef [Core.Stream]
  -> Stream a
  -> IO (WrapExpr a)
mkExpr refCount refStreams refMap e0 =
  case e0 of
    Append _ _ _    -> do s <- mkStream refCount refStreams refMap e0
                          return $ WrapExpr $ Core.drop typeOf 0 s
    Drop k e1       -> case e1 of
                         Append _ _ _ ->
                           do
                             s <- mkStream refCount refStreams refMap e1
                             return $ WrapExpr $ Core.drop typeOf k s
                         _ -> error "dfs: Drop" -- !!! This needs to be fixed !!!
    Const x         -> return $ WrapExpr $ Core.const typeOf x
    Local cs e1 e2  -> do
                         w1 <- mkExpr refCount refStreams refMap e1
                         w2 <- mkExpr refCount refStreams refMap e2
                         return $ WrapExpr $ Core.local typeOf typeOf cs
                           (unWrapExpr w1) (unWrapExpr w2)
    Var cs          -> return $ WrapExpr $ Core.var typeOf cs
    LetBinding v    -> return $ WrapExpr $ Core.letBinding typeOf v
    Extern cs       -> return $ WrapExpr $ Core.extern typeOf cs
    Op1 op e        -> do
                         w <- mkExpr refCount refStreams refMap e
                         return $ WrapExpr $ Core.op1 op (unWrapExpr w)
    Op2 op e1 e2    -> do
                         w1 <- mkExpr refCount refStreams refMap e1
                         w2 <- mkExpr refCount refStreams refMap e2
                         return $ WrapExpr $ Core.op2 op
                           (unWrapExpr w1) (unWrapExpr w2)
    Op3 op e1 e2 e3 -> do
                         w1 <- mkExpr refCount refStreams refMap e1
                         w2 <- mkExpr refCount refStreams refMap e2
                         w3 <- mkExpr refCount refStreams refMap e3
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
mkStream refCount refStreams refMap e0 =
  do
    dstn <- makeDynamicStableName e0
    let Append buf _ e = e0 -- avoids warning
    mk <- haveVisited dstn
    case mk of
      Just id_ -> return id_
      Nothing  -> addToVisited dstn buf e

  where

  {-# INLINE haveVisited #-}
  haveVisited :: DynamicStableName -> IO (Maybe Int)
  haveVisited dstn =
    do
      tab <- readIORef refStreams
      return (M.lookup dstn tab)

  {-# INLINE addToVisited #-}
  addToVisited
    :: Typed a
    => DynamicStableName
    -> [a]
    -> Stream a
    -> IO Id
  addToVisited dstn buf e =
    do
      id <- atomicModifyIORef refCount $ \ n -> (succ n, n)
      modifyIORef refStreams (M.insert dstn id)
      w <- mkExpr refCount refStreams refMap e
      modifyIORef refMap $ (:)
        Core.Stream
          { Core.streamId         = id
          , Core.streamBuffer     = buf
          , Core.streamGuard      = Nothing
          , Core.streamExpr       = unWrapExpr w
          , Core.streamExprType   = typeOf }
      return id

--------------------------------------------------------------------------------