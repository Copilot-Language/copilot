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
import Copilot.Core (DropIdx)
import Copilot.Language.Stream (Stream (..), FunArg (..))
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
  | DropMaxViolation
  | NestedExternFun
  | NestedArray
  deriving Typeable

instance Show AnalyzeException where
  show DropAppliedToNonAppend = "Drop applied to non-append operation!"
  show DropIndexOverflow      = "Drop index overflow!"
  show ReferentialCycle       = "Referential cycle!"
  show DropMaxViolation       = "Maximum drop violation (" ++ 
                                  show (maxBound :: DropIdx) ++ ")!"
  show NestedExternFun        = 
    "Extern function takes an extern function or extern array as an argument!"
  show NestedArray            = 
    "Extern array takes an extern function or extern array as an argument!"

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

analyzeTrigger :: IORef Env -> Trigger -> IO ()
analyzeTrigger refStreams (Trigger _ e0 args) =
  analyzeExpr refStreams e0 >> mapM_ analyzeTriggerArg args

  where

  analyzeTriggerArg :: TriggerArg -> IO ()
  analyzeTriggerArg (TriggerArg e) = analyzeExpr refStreams e

--------------------------------------------------------------------------------

analyzeObserver :: IORef Env -> Observer -> IO ()
analyzeObserver refStreams (Observer _ e) = analyzeExpr refStreams e

--------------------------------------------------------------------------------

data SeenExtern = NoExtern
                | SeenFun
                | SeenArr

--------------------------------------------------------------------------------

analyzeExpr :: IORef Env -> Stream a -> IO ()
analyzeExpr refStreams = go NoExtern M.empty 

  where

  go :: SeenExtern -> Env -> Stream b -> IO ()
  go seenExt nodes e0 =
    do
      dstn <- makeDynStableName e0
      assertNotVisited e0 dstn nodes
      let nodes' = M.insert dstn () nodes
      case e0 of
        Append _ _ e       -> analyzeAppend refStreams dstn e
        Const _            -> return ()
        Drop k e1          -> analyzeDrop (fromIntegral k) e1
        Local e f          -> go seenExt nodes' e >> 
                              go seenExt nodes' (f (Var "dummy"))
        Op1 _ e            -> go seenExt nodes' e
        Op2 _ e1 e2        -> go seenExt nodes' e1 >> 
                              go seenExt nodes' e2
        Op3 _ e1 e2 e3     -> go seenExt nodes' e1 >> 
                              go seenExt nodes' e2 >> 
                              go seenExt nodes' e3
        ExternFun _ args   -> case seenExt of 
                                NoExtern -> mapM_ (\(FunArg a) -> 
                                                      go SeenFun nodes' a) args
                                SeenFun  -> throw NestedExternFun
                                SeenArr  -> throw NestedArray
        ExternArray _ idx  -> case seenExt of 
                                NoExtern -> go SeenArr nodes' idx
                                SeenFun  -> throw NestedExternFun
                                SeenArr  -> throw NestedArray
        _                  -> return ()

  assertNotVisited :: Stream a -> DynStableName -> Env -> IO ()
  assertNotVisited (Append _ _ _) _    _     = return ()
  assertNotVisited _              dstn nodes =
    case M.lookup dstn nodes of
      Just () -> throw ReferentialCycle
      Nothing -> return ()

--------------------------------------------------------------------------------

analyzeAppend :: IORef Env -> DynStableName -> Stream a -> IO ()
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

analyzeDrop :: Int -> Stream a -> IO ()
analyzeDrop k (Append xs _ _)
  | k >= length xs                         = throw DropIndexOverflow
  | k > fromIntegral (maxBound :: DropIdx) = throw DropMaxViolation
  | otherwise                              = return ()
analyzeDrop _ _                            = throw DropAppliedToNonAppend

--------------------------------------------------------------------------------
