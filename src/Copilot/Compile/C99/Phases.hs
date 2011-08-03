--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Rank2Types #-}

module Copilot.Compile.C99.Phases
  ( schedulePhases
  ) where

import Copilot.Compile.C99.C2A (c2aExpr, c2aType)
import Copilot.Compile.C99.MetaTable
  (MetaTable (..), StreamInfo (..), ExternInfo (..))
import qualified Copilot.Compile.C99.Queue as Q
import qualified Copilot.Compile.C99.Witness as W
import qualified Copilot.Core as Core
import Copilot.Core.Type.Equality ((=~=), coerce, cong)
import Data.List (intersperse)
import qualified Data.Map as M
import Language.Atom (Atom, (<==), atom, cond, exactPhase)
import qualified Language.Atom as A
import Prelude hiding (id)

--------------------------------------------------------------------------------

data Phase
  = SampleExterns
  | SampleExternFuns
  | UpdateStates
  | FireTriggers
  | UpdateBuffers
  | UpdateObservers
  deriving (Bounded, Eq, Enum, Ord, Show)

numberOfPhases :: Int
numberOfPhases = succ (fromEnum (maxBound :: Phase))

--------------------------------------------------------------------------------

schedulePhases :: MetaTable -> Core.Spec -> Atom ()
schedulePhases meta spec =
  A.period numberOfPhases $
    sampleExterns    meta      >>
    sampleExternFuns meta spec >>
    updateStates     meta spec >>
    fireTriggers     meta spec >>
--    updateObservers meta spec >>
    updateBuffers    meta spec

--------------------------------------------------------------------------------

sampleExterns :: MetaTable -> Atom ()
sampleExterns =
  mapM_ sampleExtern . M.toList . externInfoMap

  where

  sampleExtern :: (Core.Name, ExternInfo) -> Atom ()
  sampleExtern (name, ExternInfo v t) =
    exactPhase (fromEnum SampleExterns) $
      atom ("sample_" ++ name) $
        do
          W.AssignInst <- return $ W.assignInst t
          v <== A.value (A.var' name (c2aType t))

--------------------------------------------------------------------------------

sampleExternFuns :: MetaTable -> Core.Spec -> Atom ()
sampleExternFuns = undefined

--------------------------------------------------------------------------------

updateStates :: MetaTable -> Core.Spec -> Atom ()
updateStates meta
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
          Just p <- return (t1 =~= t2)
          tmp <== coerce (cong p) e1

--------------------------------------------------------------------------------

fireTriggers :: MetaTable -> Core.Spec -> Atom ()
fireTriggers meta
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
      fnCall xs = name ++ "(" ++ concat (intersperse "," xs) ++ ")"

--------------------------------------------------------------------------------

updateBuffers :: MetaTable -> Core.Spec -> Atom ()
updateBuffers meta
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
