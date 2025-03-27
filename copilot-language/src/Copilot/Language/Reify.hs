-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | Transform a Copilot Language specification into a Copilot Core
-- specification.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE Safe                      #-}

module Copilot.Language.Reify
  ( reify
  ) where

import qualified Copilot.Core as Core
import Copilot.Core (Typed, Id, typeOf)

import Copilot.Language.Analyze (analyze)
import Copilot.Language.Error   (impossible)
import Copilot.Language.Spec
import Copilot.Language.Stream (Stream (..))

import Copilot.Theorem.Prove

import Prelude hiding (id)
import Data.IORef
import System.Mem.StableName.Dynamic
import System.Mem.StableName.Map (Map)
import qualified System.Mem.StableName.Map as M
import Control.Monad (liftM, unless)

-- | Transform a Copilot Language specification into a Copilot Core
-- specification.
reify :: Spec' a -> IO Core.Spec
reify spec = do
  analyze spec
  let trigs = triggers   $ runSpec spec
  let obsvs = observers  $ runSpec spec
  let props = properties $ runSpec spec
  let thms  = reverse $ theorems $ runSpec spec
  refMkId         <- newIORef 0
  refVisited      <- newIORef M.empty
  refMap          <- newIORef []
  coreTriggers    <- mapM (mkTrigger  refMkId refVisited refMap) trigs
  coreObservers   <- mapM (mkObserver refMkId refVisited refMap) obsvs
  coreProperties  <- mapM (mkProperty refMkId refVisited refMap) $ props ++ (map fst thms)
  coreStreams     <- readIORef refMap

  let cspec = Core.Spec
        { Core.specStreams    = reverse coreStreams
        , Core.specObservers  = coreObservers
        , Core.specTriggers   = coreTriggers
        , Core.specProperties = coreProperties }

  results <- sequence $ zipWith (prove cspec) (map (\(Property n _,_) -> n) thms) $ map snd thms
  unless (and results) $ putStrLn "Warning: failed to check some proofs."

  return cspec

-- | Transform a Copilot observer specification into a Copilot Core
-- observer specification.
{-# INLINE mkObserver #-}
mkObserver
  :: IORef Int
  -> IORef (Map Core.Id)
  -> IORef [Core.Stream]
  -> Observer
  -> IO Core.Observer
mkObserver refMkId refStreams refMap (Observer name e) = do
  w <- mkExpr refMkId refStreams refMap e
  return Core.Observer
    { Core.observerName     = name
    , Core.observerExpr     = w
    , Core.observerExprType = typeOf }

-- | Transform a Copilot trigger specification into a Copilot Core
-- trigger specification.
{-# INLINE mkTrigger #-}
mkTrigger
  :: IORef Int
  -> IORef (Map Core.Id)
  -> IORef [Core.Stream]
  -> Trigger
  -> IO Core.Trigger
mkTrigger refMkId refStreams refMap (Trigger name guard args) = do
  w1 <- mkExpr refMkId refStreams refMap guard
  args' <- mapM mkTriggerArg args
  return Core.Trigger
    { Core.triggerName  = name
    , Core.triggerGuard = w1
    , Core.triggerArgs  = args' }

  where

  mkTriggerArg :: Arg -> IO Core.UExpr
  mkTriggerArg (Arg e) = do
    w <- mkExpr refMkId refStreams refMap e
    return $ Core.UExpr typeOf w

-- | Transform a Copilot property specification into a Copilot Core
-- property specification.
{-# INLINE mkProperty #-}
mkProperty
  :: IORef Int
  -> IORef (Map Core.Id)
  -> IORef [Core.Stream]
  -> Property
  -> IO Core.Property
mkProperty refMkId refStreams refMap (Property name p) = do
  p' <- mkProp refMkId refStreams refMap p
  return Core.Property
    { Core.propertyName  = name
    , Core.propertyProp  = p' }

-- | Transform a Copilot proposition into a Copilot Core proposition.
mkProp :: IORef Int
       -> IORef (Map Core.Id)
       -> IORef [Core.Stream]
       -> Prop a
       -> IO Core.Prop
mkProp refMkId refStreams refMap prop =
  case prop of
    Forall e -> Core.Forall <$> mkExpr refMkId refStreams refMap e
    Exists e -> Core.Exists <$> mkExpr refMkId refStreams refMap e

-- | Transform a Copilot stream expression into a Copilot Core expression.
{-# INLINE mkExpr #-}
mkExpr
  :: Typed a
  => IORef Int
  -> IORef (Map Core.Id)
  -> IORef [Core.Stream]
  -> Stream a
  -> IO (Core.Expr a)
mkExpr refMkId refStreams refMap = go

  where
  go :: Typed a => Stream a -> IO (Core.Expr a)
  go e0 = case e0 of

    ------------------------------------------------------

    Append _ _ _ -> do
      s <- mkStream refMkId refStreams refMap e0
      return $ Core.Drop typeOf 0 s

    ------------------------------------------------------

    Drop k e1 -> case e1 of
      Append _ _ _ -> do
        s <- mkStream refMkId refStreams refMap e1
        return $ Core.Drop typeOf (fromIntegral k) s
      _ -> impossible "mkExpr" "copilot-language"

    ------------------------------------------------------

    Const x -> return $ Core.Const typeOf x

    ------------------------------------------------------

    Local e f -> do
        id <- mkId refMkId
        let cs = "local_" ++ show id
        w1 <- go e
        w2 <- go (f (Var cs))
        return $ Core.Local typeOf typeOf cs w1 w2

    ------------------------------------------------------

    Label s e -> do
        w <- go e
        return $ Core.Label typeOf s w

    ------------------------------------------------------

    Var cs -> return $ Core.Var typeOf cs

    ------------------------------------------------------

    Extern cs mXs -> return $ Core.ExternVar typeOf cs mXs

    ------------------------------------------------------

    Op1 op e -> do
      w <- go e
      return $ Core.Op1 op w

    ------------------------------------------------------

    Op2 op e1 e2 -> do
      w1 <- go e1
      w2 <- go e2
      return $ Core.Op2 op w1 w2

    ------------------------------------------------------

    Op3 op e1 e2 e3 -> do
      w1 <- go e1
      w2 <- go e2
      w3 <- go e3
      return $ Core.Op3 op w1 w2 w3

    ------------------------------------------------------

  mkFunArg :: Arg -> IO Core.UExpr
  mkFunArg (Arg e) = do
    w <- mkExpr refMkId refStreams refMap e
    return $ Core.UExpr typeOf w

  mkStrArg :: (Core.Name, Arg) -> IO (Core.Name, Core.UExpr)
  mkStrArg (name, Arg e) = do
    w <- mkExpr refMkId refStreams refMap e
    return $ (name, Core.UExpr typeOf w)

-- | Transform a Copilot stream expression into a Copilot Core stream
-- expression.
{-# INLINE mkStream #-}
mkStream
  :: Typed a
  => IORef Int
  -> IORef (Map Core.Id)
  -> IORef [Core.Stream]
  -> Stream a
  -> IO Id
mkStream refMkId refStreams refMap e0 = do
  dstn <- makeDynStableName e0
  let Append buf _ e = e0 -- avoids warning
  mk <- haveVisited dstn
  case mk of
    Just id_ -> return id_
    Nothing  -> addToVisited dstn buf e

  where

  {-# INLINE haveVisited #-}
  haveVisited :: DynStableName -> IO (Maybe Int)
  haveVisited dstn = do
    tab <- readIORef refStreams
    return (M.lookup dstn tab)

  {-# INLINE addToVisited #-}
  addToVisited
    :: Typed a
    => DynStableName
    -> [a]
    -> Stream a
    -> IO Id
  addToVisited dstn buf e = do
    id <- mkId refMkId
    modifyIORef refStreams (M.insert dstn id)
    w <- mkExpr refMkId refStreams refMap e
    modifyIORef refMap $ (:)
      Core.Stream
        { Core.streamId         = id
        , Core.streamBuffer     = buf
        , Core.streamExpr       = w
        , Core.streamExprType   = typeOf }
    return id

-- | Create a fresh, unused 'Id'.
mkId :: IORef Int -> IO Id
mkId refMkId = atomicModifyIORef refMkId $ \ n -> (succ n, n)
