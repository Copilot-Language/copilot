--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

-- | Copilot specification sanity check.

module Copilot.Language.Analyze
  ( AnalyzeException (..)
  , analyze
  ) where

import Control.Exception (Exception, throw)
import Copilot.Language.Spec
import Copilot.Language.Stream (Stream (..))
import Data.IORef
import Data.Typeable
import System.Mem.StableName.Dynamic
import System.Mem.StableName.Dynamic.Map (Map)
import qualified System.Mem.StableName.Dynamic.Map as M

--------------------------------------------------------------------------------

data AnalyzeException
  = DropAppliedToNonAppend
  | DropIndexOverflow
  | ReferentialCycle
  deriving Typeable

instance Show AnalyzeException where
  show DropAppliedToNonAppend = "Drop applied to non-append operation!"
  show DropIndexOverflow      = "Drop index overflow!"
  show ReferentialCycle       = "Referential cycle!"

instance Exception AnalyzeException

--------------------------------------------------------------------------------

type Env = Map ()

--------------------------------------------------------------------------------

analyze :: Spec -> IO ()
analyze spec =
  do
    refStreams <- newIORef M.empty
    mapM_ (analyzeTrigger  refStreams) (triggers  (runSpec spec))
    mapM_ (analyzeObserver refStreams) (observers (runSpec spec))

--------------------------------------------------------------------------------

{-# INLINE analyzeTrigger #-}
analyzeTrigger :: IORef Env -> Trigger -> IO ()
analyzeTrigger refStreams (Trigger _ e0 args) =
  analyzeExpr refStreams e0 >> mapM_ analyzeTriggerArg args

  where

  {-# INLINE analyzeTriggerArg #-}
  analyzeTriggerArg :: TriggerArg -> IO ()
  analyzeTriggerArg (TriggerArg e) = analyzeExpr refStreams e

--------------------------------------------------------------------------------

{-# INLINE analyzeObserver #-}
analyzeObserver :: IORef Env -> Observer -> IO ()
analyzeObserver refStreams (Observer _ e) = analyzeExpr refStreams e

--------------------------------------------------------------------------------

{-# INLINE analyzeExpr #-}
analyzeExpr :: IORef Env -> Stream a -> IO ()
analyzeExpr refStreams = go M.empty

  where

  go :: Env -> Stream b -> IO ()
  go nodes e0 =
    do
      dstn <- makeDynamicStableName e0
      assertNotVisited e0 dstn nodes
      let nodes' = M.insert dstn () nodes
      case e0 of
        Append _ _ e   -> analyzeAppend refStreams dstn e
        Const _        -> return ()
        Drop k e1      -> analyzeDrop (fromIntegral k) e1
        Local e f      -> go nodes' e >> go nodes' (f (Var "dummy"))
        Op1 _ e        -> go nodes' e
        Op2 _ e1 e2    ->
                          do
                            go nodes' e1
                            go nodes' e2
        Op3 _ e1 e2 e3 -> go nodes' e1 >> go nodes' e2 >> go nodes' e3
        _              -> return ()

  {-# INLINE assertNotVisited #-}
  assertNotVisited :: Stream a -> DynamicStableName -> Env -> IO ()
  assertNotVisited (Append _ _ _) _    _     = return ()
  assertNotVisited _              dstn nodes =
    case M.lookup dstn nodes of
      Just () -> throw ReferentialCycle
      Nothing -> return ()

--------------------------------------------------------------------------------

{-# INLINE analyzeAppend #-}
analyzeAppend :: IORef Env -> DynamicStableName -> Stream a -> IO ()
analyzeAppend refStreams dstn e =
  do
    streams <- readIORef refStreams
    case M.lookup dstn streams of
      Just () -> return ()
      Nothing ->
        do
          modifyIORef refStreams $ M.insert dstn ()
          analyzeExpr refStreams e

--------------------------------------------------------------------------------

{-# INLINE analyzeDrop #-}
analyzeDrop :: Int -> Stream a -> IO ()
analyzeDrop k (Append xs _ _)
  | k >= length xs = throw DropIndexOverflow
  | otherwise      = return ()
analyzeDrop _ _    = throw DropAppliedToNonAppend

--------------------------------------------------------------------------------