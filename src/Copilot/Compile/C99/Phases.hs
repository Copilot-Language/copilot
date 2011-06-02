--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.Phases
  ( schedulePhases
  ) where

import Copilot.Compile.C99.C2A (c2aExpr, c2aType)
import Copilot.Compile.C99.MetaTable
  (MetaTable (..), StreamInfo (..), ExternInfo (..))
import qualified Copilot.Compile.C99.Witness as W
import qualified Copilot.Core as Core
import Copilot.Core.Type.Equality ((=~=), coerce, cong)
import Language.Atom (Atom, (<==), (>=.), atom, action, cond, exactPhase, value)
import qualified Language.Atom as Atom
import Data.List (intersperse)
import qualified Data.Map as M
import Prelude hiding (id)

--------------------------------------------------------------------------------

data Phase
  = SampleExterns
  | UpdateStates
  | FireTriggers
  | UpdateBuffers
  deriving (Bounded, Eq, Enum, Ord, Show)

numberOfPhases :: Int
numberOfPhases = 4 -- succ (fromEnum (maxBound :: Phase))

--------------------------------------------------------------------------------

schedulePhases :: MetaTable -> Core.Spec -> Atom ()
schedulePhases meta spec =
  Atom.period numberOfPhases $
    sampleExterns meta      >>
    updateStates  meta spec >>
    fireTriggers  meta spec >>
    updateBuffers meta spec

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
          v <== Atom.value (Atom.var' name (c2aType t))

--------------------------------------------------------------------------------

updateStates :: MetaTable -> Core.Spec -> Atom ()
updateStates meta
  Core.Spec
    { Core.specStreams = streams
    } =
  mapM_ updateState streams

  where

  updateState :: Core.Stream -> Atom ()
  updateState
    Core.Stream
      { Core.streamId       = id
      , Core.streamExpr     = e
      , Core.streamExprType = t1
      } =
    let
      Just strmInfo = M.lookup id (streamInfoMap meta)
    in
      updateState1 t1 id (c2aExpr meta e) strmInfo

  updateState1 :: Core.Type α -> Core.Id -> Atom.E α -> StreamInfo -> Atom ()
  updateState1 t1 id e1
    StreamInfo
      { streamInfoTempVar = tmp
      , streamInfoType    = t2
      } =
    exactPhase (fromEnum UpdateStates) $
      atom ("update_state_s" ++ show id) $
        do
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
          cond (c2aExpr meta e0)
          action fnCall (reverse $ fmap triggerArg2UE args)

      where

      triggerArg2UE :: Core.TriggerArg -> Atom.UE
      triggerArg2UE (Core.TriggerArg e t) =
        case W.exprInst t of
          W.ExprInst ->
            Atom.ue $ c2aExpr meta e

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
      { streamInfoBufferArray = arr
      , streamInfoBufferIndex = idx
      , streamInfoTempVar     = tmp
      , streamInfoBufferSize  = sz
      , streamInfoType        = t
      } =
    exactPhase (fromEnum UpdateBuffers) $
      atom ("update_buffer_s" ++ show id) $
        do
          W.AssignInst <- return (W.assignInst t)
          (arr Atom.! value idx) <== value tmp
          idx <== Atom.mux (value idx + 1 >=. sz) 0 (value idx + 1)

--------------------------------------------------------------------------------