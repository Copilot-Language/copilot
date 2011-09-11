--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs, Rank2Types #-}

module Copilot.Compile.C99.Phases
  ( schedulePhases
  , numberOfPhases
  ) where

import Copilot.Compile.C99.C2A (c2aExpr, c2aType)
import Copilot.Compile.C99.MetaTable
  (MetaTable (..), StreamInfo (..), ExternInfo (..), ExternArrayInfo (..))
import Copilot.Compile.C99.Params
import qualified Copilot.Compile.C99.Queue as Q
import qualified Copilot.Compile.C99.Witness as W
import qualified Copilot.Core as Core
import Copilot.Core.Type.Equality ((=~=), Equal (..))
import Data.List (intersperse)
import qualified Data.Map as M
import Language.Atom (Atom, (<==), atom, cond, exactPhase)
import qualified Language.Atom as A
import Prelude hiding (id)

--------------------------------------------------------------------------------

data Phase
  = SampleExternVars
  | SampleExternArrays
  | SampleExternFuns
  | UpdateStates
  | FireTriggers
  | UpdateBuffers
  | UpdateObservers
  deriving (Bounded, Eq, Enum, Ord, Show)

numberOfPhases :: Int
numberOfPhases = succ (fromEnum (maxBound :: Phase))

--------------------------------------------------------------------------------

schedulePhases :: Params -> MetaTable -> Core.Spec -> Atom ()
schedulePhases params meta spec =
  A.period numberOfPhases $
    sampleExternVars    params meta spec >>
    sampleExternArrays  params meta spec >>
    sampleExternFuns    params meta spec >>
    updateStates        params meta spec >>
    fireTriggers        params meta spec >>
    updateObservers     params meta spec >>
    updateBuffers       params meta spec

--------------------------------------------------------------------------------

sampleExternVars :: Params -> MetaTable -> Core.Spec -> Atom ()
sampleExternVars _ spec _ =
  (mapM_ sampleExternVar . M.toList . externInfoMap) spec

  where

  sampleExternVar :: (Core.Name, ExternInfo) -> Atom ()
  sampleExternVar (name, ExternInfo v t) =
    exactPhase (fromEnum SampleExternVars) $
      atom ("sample_" ++ name) $
        do
          W.AssignInst <- return $ W.assignInst t
          v <== A.value (A.var' name (c2aType t))

--------------------------------------------------------------------------------

sampleExternArrays :: Params -> MetaTable -> Core.Spec -> Atom ()
sampleExternArrays _ spec _ =
  (mapM_ sampleExternArray . M.toList . externArrayInfoMap) spec

  where

  sampleExternArray :: (Core.Name, ExternArrayInfo) -> Atom ()
  sampleExternArray (_, ExternArrayInfo _ _ _) = undefined

--------------------------------------------------------------------------------

sampleExternFuns :: Params -> MetaTable -> Core.Spec -> Atom ()
sampleExternFuns _ _ _ = return ()

--------------------------------------------------------------------------------

updateStates :: Params -> MetaTable -> Core.Spec -> Atom ()
updateStates _ meta
  Core.Spec
    { Core.specStreams = streams
    } = mapM_ updateStreamState streams

  where

  updateStreamState :: Core.Stream -> Atom ()
  updateStreamState
    Core.Stream
      { Core.streamId       = id
      , Core.streamExpr     = e
      , Core.streamExprType = t1
      , Core.streamGuard    = g
      } =
    do
      let e' = c2aExpr meta e
          Just strmInfo = M.lookup id (streamInfoMap meta)
          g' = cond (c2aExpr meta g)
      updateStreamState1 t1 id e' g' strmInfo

  updateStreamState1
    :: Core.Type a -> Core.Id -> A.E a -> Atom () -> StreamInfo -> Atom ()
  updateStreamState1 t1 id e1 g1
    StreamInfo
      { streamInfoTempVar = tmp
      , streamInfoType    = t2
      } =
    exactPhase (fromEnum UpdateStates) $
      atom ("update_state_s" ++ show id) $
        do
          g1
          W.AssignInst <- return (W.assignInst t2)
          Just Refl <- return (t1 =~= t2)
          tmp <== e1 -- coerce (cong p) e1

--------------------------------------------------------------------------------

fireTriggers :: Params -> MetaTable -> Core.Spec -> Atom ()
fireTriggers params meta
  Core.Spec
    { Core.specTriggers = triggers
    } =
  mapM_ fireTrigger triggers

  where

  fireTrigger :: Core.Trigger -> Atom ()
  fireTrigger
    Core.Trigger
      { Core.triggerName  = name
      , Core.triggerGuard = e0
      , Core.triggerArgs  = args
      } =
    exactPhase (fromEnum FireTriggers) $
      atom ("fire_trigger_" ++ name) $
        do
          let args' = map triggerArg2UE (reverse args)
              e0'   = c2aExpr meta e0
          cond e0'
          A.action fnCall args'

      where

      triggerArg2UE :: Core.UExpr -> A.UE
      triggerArg2UE (Core.UExpr t e) =
        case W.exprInst t of
          W.ExprInst -> A.ue (c2aExpr meta e)

      fnCall :: [String] -> String
      fnCall xs = withPrefix (prefix params) name ++
        "(" ++ concat (intersperse "," xs) ++ ")"

--------------------------------------------------------------------------------

updateObservers :: Params -> MetaTable -> Core.Spec -> Atom ()
updateObservers params meta
  Core.Spec
    { Core.specObservers = observers
    } =
  mapM_ updateObserver observers

  where

  updateObserver :: Core.Observer -> Atom ()
  updateObserver
    Core.Observer
      { Core.observerName     = name
      , Core.observerExpr     = e
      , Core.observerExprType = t
      } =
    exactPhase (fromEnum UpdateObservers) $
      atom ("update_observer_" ++ name) $
        do
          let e' = c2aExpr meta e
          W.AssignInst <- return (W.assignInst t)
          (A.var' (withPrefix (prefix params) name) . c2aType) t <== e'

--------------------------------------------------------------------------------

updateBuffers :: Params -> MetaTable -> Core.Spec -> Atom ()
updateBuffers _ meta
  Core.Spec
    { Core.specStreams = streams
    } =
  mapM_ updateBuffer streams

  where

  updateBuffer :: Core.Stream -> Atom ()
  updateBuffer
    Core.Stream
      { Core.streamId = id
      } =
    let
      Just strmInfo = M.lookup id (streamInfoMap meta)
    in
      updateBuffer1 id strmInfo

  updateBuffer1 :: Core.Id -> StreamInfo -> Atom ()
  updateBuffer1 id
    StreamInfo
      { streamInfoQueue      = que
      , streamInfoTempVar    = tmp
      , streamInfoType       = t
      } =
    exactPhase (fromEnum UpdateBuffers) $
      atom ("update_buffer_s" ++ show id) $
        do
          W.AssignInst <- return (W.assignInst t)
          Q.dropFirstElemAndSnoc (A.value tmp) que
