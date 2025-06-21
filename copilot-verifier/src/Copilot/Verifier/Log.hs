{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Copilot.Verifier.Log
  ( SupportsCopilotLogMessage
  , CopilotLogMessage(..)
  , VerificationStep(..)
  , StateRelationStep(..)
  , VerifierAssertion(..)
  , SomeSome(..)
  , StreamValueEquality(..)
  , TriggersInvokedCorrespondingly(..)
  , TriggerArgumentEquality(..)
  , RingBufferLoad(..)
  , RingBufferIndexLoad(..)
  , PointerArgumentLoad(..)
  , AccessorFunctionLoad(..)
  , GuardFunctionLoad(..)
  , UnknownFunctionLoad(..)
  , LLVMBadBehaviorCheck(..)
  , sayCopilot
  , copilotLogMessageToSayWhat
  ) where

import Crux (SayLevel (..), SayWhat (..))
import qualified Crux.Log as Log
import Data.Aeson (ToJSON (..), Value (..))
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.Kind (Type)
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Vector as PV
import qualified Data.Parameterized.TraversableFC.WithIndex as PWI
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP

import qualified Copilot.Core.Expr as CE
import qualified Copilot.Core.Type as CT
import qualified Copilot.Theorem.What4 as CW4

import qualified Lang.Crucible.Simulator as LCS
import qualified Lang.Crucible.Types as LCT
import qualified Lang.Crucible.LLVM.Errors as LCLE
import qualified Lang.Crucible.LLVM.Errors.MemoryError as LCLEME
import qualified Lang.Crucible.LLVM.Errors.UndefinedBehavior as LCLEUB
import qualified Lang.Crucible.LLVM.MemModel as LCLM
import qualified Lang.Crucible.LLVM.MemModel.CallStack as LCLMCS
import qualified What4.FunctionName as WF
import qualified What4.Interface as WI
import qualified What4.ProgramLoc as WPL

data CopilotLogMessage where
  GeneratedCFile ::
       FilePath
       -- ^ The path of the generated C File
    -> CopilotLogMessage
  CompiledBitcodeFile ::
       String
       -- ^ The prefix to use in the compiled bitcode's directory
    -> FilePath
       -- ^ The name of the generated LLVM bitcode file
    -> CopilotLogMessage
  TranslatedToCrucible :: CopilotLogMessage
  GeneratingProofState :: CopilotLogMessage
  ComputingConditions :: VerificationStep -> CopilotLogMessage
  ProvingConditions :: VerificationStep -> CopilotLogMessage
  AllGoalsProved :: CopilotLogMessage
  OnlySomeGoalsProved ::
       Integer
       -- ^ Number of goals proved
    -> Integer
       -- ^ Number of total goals
    -> CopilotLogMessage
  SuccessfulProofSummary ::
       FilePath
       -- ^ Name of the generated C file.
    -> Integer
       -- ^ Number of goals proven during for the initial state of the proof.
    -> Integer
       -- ^ Number of goals proven during the bisimulation step of the proof.
    -> CopilotLogMessage
  NoisyVerbositySuggestion :: CopilotLogMessage

  -----
  -- Types of proof goals the verifier emits
  --
  -- The first three arguments to each constructor are:
  --
  -- * Which step of the verifier we are on
  -- * The current goal number (zero-indexed)
  -- * The total number of goals
  -----

  StreamValueEqualityProofGoal ::
       WI.IsSymExprBuilder sym
    => VerificationStep
    -> Integer
    -> Integer
    -> StreamValueEquality sym copilotType crucibleType
    -> CopilotLogMessage

  TriggersInvokedCorrespondinglyProofGoal ::
       WI.IsSymExprBuilder sym
    => VerificationStep
    -> Integer
    -> Integer
    -> TriggersInvokedCorrespondingly sym
    -> CopilotLogMessage

  TriggerArgumentEqualityProofGoal ::
       WI.IsSymExprBuilder sym
    => VerificationStep
    -> Integer
    -> Integer
    -> TriggerArgumentEquality sym copilotType crucibleType
    -> CopilotLogMessage

  RingBufferLoadProofGoal ::
       WI.IsSymExprBuilder sym
    => VerificationStep
    -> Integer
    -> Integer
    -> RingBufferLoad sym copilotType crucibleType
    -> CopilotLogMessage

  RingBufferIndexLoadProofGoal ::
       WI.IsSymExprBuilder sym
    => VerificationStep
    -> Integer
    -> Integer
    -> RingBufferIndexLoad sym
    -> CopilotLogMessage

  PointerArgumentLoadProofGoal ::
       WI.IsSymExprBuilder sym
    => VerificationStep
    -> Integer
    -> Integer
    -> PointerArgumentLoad sym copilotType crucibleType
    -> CopilotLogMessage

  AccessorFunctionLoadProofGoal ::
       WI.IsSymExprBuilder sym
    => VerificationStep
    -> Integer
    -> Integer
    -> AccessorFunctionLoad sym
    -> CopilotLogMessage

  GuardFunctionLoadProofGoal ::
       WI.IsSymExprBuilder sym
    => VerificationStep
    -> Integer
    -> Integer
    -> GuardFunctionLoad sym
    -> CopilotLogMessage

  UnknownFunctionLoadProofGoal ::
       WI.IsSymExprBuilder sym
    => VerificationStep
    -> Integer
    -> Integer
    -> UnknownFunctionLoad sym
    -> CopilotLogMessage

  LLVMBadBehaviorCheckProofGoal ::
       WI.IsSymExprBuilder sym
    => VerificationStep
    -> Integer
    -> Integer
    -> LLVMBadBehaviorCheck sym
    -> CopilotLogMessage

data VerificationStep
  = InitialState
  | StepBisimulation
  deriving stock Generic
  deriving anyclass ToJSON

-- | The provenance of an assertion.
data AssertionProvenance
  = VerifierProvenance
    -- ^ Assertions that the verifier directly makes to check the correspondence
    -- between the Copilot specification and the generated C code. Because these
    -- assertions are core to the proof that the verifier is computing, these
    -- assertions' proof goals are always logged, even if they are trivial.
  | StepExecutionProvenance
    -- ^ Assertions that arise during symbolic execution of the @step()@
    -- function as part of proving the memory safety of the generated C code.
    -- These assertions' proof goals are only logged if they are non-trivial,
    -- as there would be an excessive number of these proof goals otherwise.

-- | At what step of the proof are we checking the state relation? We record
-- this so that we can better distinguish between transition step–related
-- proof goals that arise before or after calling the @step()@ function.
data StateRelationStep
  = InitialStateRelation
    -- ^ Check the state relation for the initial state.
  | PreStepStateRelation
    -- ^ During the transition step of the proof, check the state relation
    -- before calling the @step()@ function.
  | PostStepStateRelation
    -- ^ During the transition step of the proof, check the state relation
    -- after calling the @step()@ function.
  deriving stock Generic
  deriving anyclass ToJSON

-- | Types of assertions that the verifier can make, which will count towards
-- the total number of proof goals.
data VerifierAssertion sym
  = StreamValueEqualityAssertion (SomeSome (StreamValueEquality sym))
  | TriggersInvokedCorrespondinglyAssertion (TriggersInvokedCorrespondingly sym)
  | TriggerArgumentEqualityAssertion (SomeSome (TriggerArgumentEquality sym))
  | RingBufferLoadAssertion (SomeSome (RingBufferLoad sym))
  | RingBufferIndexLoadAssertion (RingBufferIndexLoad sym)
  | PointerArgumentLoadAssertion (SomeSome (PointerArgumentLoad sym))
  | AccessorFunctionLoadAssertion (AccessorFunctionLoad sym)
  | GuardFunctionLoadAssertion (GuardFunctionLoad sym)
  | UnknownFunctionLoadAssertion (UnknownFunctionLoad sym)
  | LLVMBadBehaviorCheckAssertion (LLVMBadBehaviorCheck sym)

-- | Like @Some@ in @parameterized-utils@, but existentially closing over two
-- type parameters instead of just one.
data SomeSome (f :: j -> k -> Type) where
  SomeSome :: f x y -> SomeSome f

-- | An assertion that an element in a Copilot stream is equal to the
-- corresponding element in a C ring buffer.
data StreamValueEquality sym copilotType crucibleType where
  StreamValueEquality ::
       sym
    -> StateRelationStep
       -- ^ When the values are checked for equality
    -> WPL.ProgramLoc
       -- ^ The locations of the values
    -> Text
       -- ^ The name of the buffer
    -> Integer
       -- ^ The offset from the buffer's index, which is used to compute the
       -- element of the buffer to load
    -> Integer
       -- ^ The number of elements in the buffer
    -> CT.Type copilotType
       -- ^ The Copilot type
    -> CW4.XExpr sym
       -- ^ The Copilot value
    -> LCT.TypeRepr crucibleType
       -- ^ The Crucible type
    -> LCS.RegValue sym crucibleType
       -- ^ The Crucible value
    -> StreamValueEquality sym copilotType crucibleType

-- | An assertion that, given a Copilot trigger stream and its corresponding C
-- trigger function on a particular time step, either both fired at the same
-- time or both did not fire at all.
data TriggersInvokedCorrespondingly sym where
  TriggersInvokedCorrespondingly ::
       WPL.ProgramLoc
       -- ^ The location of the trigger
    -> CE.Name
       -- ^ The trigger name
    -> WI.SymNat sym
       -- ^ The expected number of times the trigger was fired this step
       -- (should be either 1 or 0).
    -> WI.SymNat sym
       -- ^ The actual number of times the trigger was fired this step.
    -> TriggersInvokedCorrespondingly sym

-- | An assertion that an argument to a Copilot trigger is equal to the
-- corresponding argument to a C trigger function.
data TriggerArgumentEquality sym copilotType crucibleType where
  TriggerArgumentEquality ::
       sym
    -> WPL.ProgramLoc
       -- ^ The locations of the arguments
    -> CE.Name
       -- ^ The trigger name
    -> Integer
       -- ^ The number of the argument (starting from 0)
    -> CT.Type copilotType
       -- ^ The Copilot type
    -> CW4.XExpr sym
       -- ^ The Copilot value
    -> LCT.TypeRepr crucibleType
       -- ^ The Crucible type
    -> LCS.RegValue sym crucibleType
       -- ^ The Crucible value
    -> TriggerArgumentEquality sym copilotType crucibleType

-- | An assertion that a load from a ring buffer in C is valid.
data RingBufferLoad sym copilotType crucibleType where
  RingBufferLoad ::
       sym
    -> StateRelationStep
       -- ^ When the ring buffer is loaded from
    -> WPL.ProgramLoc
       -- ^ The location of the buffer
    -> Text
       -- ^ The name of the buffer
    -> Integer
       -- ^ The offset from the buffer's index, which is used to compute the
       -- element of the buffer to load
    -> Integer
       -- ^ The number of elements in the buffer
    -> CT.Type copilotType
       -- ^ The Copilot type of the elements of the array
    -> LCT.TypeRepr crucibleType
       -- ^ The Crucible type of the elements of the array
    -> WI.Pred sym
       -- ^ The assertion that must hold in order for this load to be valid
    -> RingBufferLoad sym copilotType crucibleType

-- | An assertion that a load from a global variable representing a ring
-- buffer's index in C is valid.
data RingBufferIndexLoad sym where
  RingBufferIndexLoad ::
       sym
    -> StateRelationStep
       -- ^ When the index's global variable is loaded from
    -> WPL.ProgramLoc
       -- ^ The location of the global index
    -> Text
       -- ^ The name of the global index
    -> WI.Pred sym
       -- ^ The assertion that must hold in order for this load to be valid
    -> RingBufferIndexLoad sym

-- | An assertion that a load from a pointer argument to a trigger function in C
-- is valid.
data PointerArgumentLoad sym copilotType crucibleType where
  PointerArgumentLoad ::
       sym
    -> WPL.ProgramLoc
       -- ^ The location of the pointer
    -> CT.Type copilotType
       -- ^ The Copilot type of the underlying memory
    -> LCT.TypeRepr crucibleType
       -- ^ The Crucible type of the underlying memory
    -> WI.Pred sym
       -- ^ The assertion that must hold in order for this load to be valid
    -> PointerArgumentLoad sym copilotType crucibleType

-- | An assertion that a load occurring from somewhere in a stream accessor
-- function in C (e.g., @s0_get@) is valid. This is a somewhat imprecise
-- assertion, as it doesn't identify /why/ the load occurs. (Most likely, it
-- happens because of an array index.)
data AccessorFunctionLoad sym where
  AccessorFunctionLoad ::
       sym
    -> WPL.ProgramLoc
       -- ^ The location of the accessor function
    -> WF.FunctionName
       -- ^ The name of the accessor function
    -> WI.Pred sym
       -- ^ The assertion that must hold in order for this load to be valid
    -> AccessorFunctionLoad sym

-- | An assertion that a load occurring from somewhere in a trigger guard
-- function in C (e.g., @even_guard@) is valid. This is a somewhat imprecise
-- assertion, as it doesn't identify /why/ the load occurs. (Most likely, it
-- happens because of an array index.)
data GuardFunctionLoad sym where
  GuardFunctionLoad ::
       sym
    -> WPL.ProgramLoc
       -- ^ The location of the guard function
    -> WF.FunctionName
       -- ^ The name of the guard function
    -> WI.Pred sym
       -- ^ The assertion that must hold in order for this load to be valid
    -> GuardFunctionLoad sym

-- | An assertion that a load occurring in some function is valid. If you
-- see this assertion, it is because the heuristics used to identify where
-- load-related assertions come from could not identify a more precise cause
-- for a load.
data UnknownFunctionLoad sym where
  UnknownFunctionLoad ::
       sym
    -> WPL.ProgramLoc
       -- ^ The location of the function
    -> WF.FunctionName
       -- ^ The name of the function
    -> WI.Pred sym
       -- ^ The assertion that must hold in order for this load to be valid
    -> UnknownFunctionLoad sym

-- | An assertion that checks that some form of bad behavior in LLVM does not
-- occur. Bad behavior includes both undefined behavior and memory unsafety.
data LLVMBadBehaviorCheck sym where
  LLVMBadBehaviorCheck ::
       sym
    -> WPL.ProgramLoc
       -- ^ The location of the check
    -> LCLMCS.CallStack
       -- ^ A call stack for the check, if one exists
    -> LCLE.BadBehavior sym
       -- ^ What type of LLVM bad behavior is being checked for
    -> WI.Pred sym
       -- ^ The assertion that must hold in order for this check to succeed
    -> LLVMBadBehaviorCheck sym

-- Silly ToJSON instances. Crux is only requiring a ToJSON constraint for
-- IDE-related functionality that we do not make use of, so the behavior of
-- these instances aren't very important.

instance ToJSON (StreamValueEquality sym copilotType crucibleType) where
  toJSON _ = String "StreamValueEquality"

instance ToJSON (TriggersInvokedCorrespondingly sym) where
  toJSON _ = String "TriggersInvokedCorrespondingly"

instance ToJSON (TriggerArgumentEquality sym copilotType crucibleType) where
  toJSON _ = String "TriggerArgumentEquality"

instance ToJSON (RingBufferLoad sym copilotType crucibleType) where
  toJSON _ = String "RingBufferLoad"

instance ToJSON (RingBufferIndexLoad sym) where
  toJSON _ = String "RingBufferIndexLoad"

instance ToJSON (PointerArgumentLoad sym copilotType crucibleType) where
  toJSON _ = String "PointerArgumentLoad"

instance ToJSON (AccessorFunctionLoad sym) where
  toJSON _ = String "AccessorFunctionLoad"

instance ToJSON (GuardFunctionLoad sym) where
  toJSON _ = String "GuardFunctionLoad"

instance ToJSON (UnknownFunctionLoad sym) where
  toJSON _ = String "UnknownFunctionLoad"

instance ToJSON (LLVMBadBehaviorCheck sym) where
  toJSON _ = String "LLVMBadBehaviorCheck"

type SupportsCopilotLogMessage msgs =
  (?injectCopilotLogMessage :: CopilotLogMessage -> msgs)

sayCopilot ::
  Log.Logs msgs =>
  SupportsCopilotLogMessage msgs =>
  CopilotLogMessage ->
  IO ()
sayCopilot msg =
  let ?injectMessage = ?injectCopilotLogMessage
   in Log.say msg

copilotTag :: Text
copilotTag = "copilot-verifier"

-- copilotFail :: Text -> SayWhat
-- copilotFail = SayWhat Fail copilotTag

copilotSimply :: Text -> SayWhat
copilotSimply = SayWhat Simply copilotTag

copilotNoisily :: Text -> SayWhat
copilotNoisily = SayWhat Noisily copilotTag

-- copilotWarn :: Text -> SayWhat
-- copilotWarn = SayWhat Warn copilotTag

copilotLogMessageToSayWhat :: CopilotLogMessage -> SayWhat
copilotLogMessageToSayWhat (GeneratedCFile csrc) =
  copilotSimply $ "Generated " <> T.pack (show csrc)
copilotLogMessageToSayWhat (CompiledBitcodeFile prefix bcFile) =
  copilotSimply $ "Compiled " <> T.pack prefix <> " into " <> T.pack bcFile
copilotLogMessageToSayWhat TranslatedToCrucible =
  copilotSimply "Translated bitcode into Crucible"
copilotLogMessageToSayWhat GeneratingProofState =
  copilotSimply "Generating proof state data"
copilotLogMessageToSayWhat (ComputingConditions step) =
  copilotSimply $ "Computing " <> describeVerificationStep step <> " verification conditions"
copilotLogMessageToSayWhat (ProvingConditions step) =
  copilotSimply $ "Proving " <> describeVerificationStep step <> " verification conditions"
copilotLogMessageToSayWhat AllGoalsProved =
  copilotSimply "All obligations proved by concrete simplification"
copilotLogMessageToSayWhat (OnlySomeGoalsProved numProvedGoals numTotalGoals) =
  copilotSimply $ T.unwords
    [ "Proved", T.pack (show numProvedGoals)
    , "of"
    , T.pack (show numTotalGoals), "total goals"
    ]
copilotLogMessageToSayWhat (SuccessfulProofSummary cFileName initGoals bisimGoals) =
  copilotSimply $ T.unlines
    [ ""
    , "copilot-verifier has produced a mathematical proof that the behavior of"
    , "the generated C program (" <> T.pack cFileName
      <> ") precisely matches the behavior of"
    , "the Copilot specification. This proof shows that the behaviors match for"
    , "all possible moments in time, and in doing so, copilot-verifier examined"
    , "how the programs behave at the start of execution (the \"initial state\")"
    , "and at an arbitrary point of time in execution (the \"step bisimulation\")."
    , "copilot-verifier decomposed the overall proof into smaller goals, the"
    , "number of which depends on the size and complexity of the program. In this"
    , "example, there are " <> T.pack (show initGoals) <> " initial state goal(s)"
      <> " and " <> T.pack (show bisimGoals) <> " step bisimulation goal(s),"
    , "and copilot-verifier was able to prove all of them."
    ]
copilotLogMessageToSayWhat NoisyVerbositySuggestion =
  copilotSimply $ T.unlines
    [ ""
    , "copilot-verifier is displaying an abridged summary of the verification results."
    , "copilot-verifier also includes a \"noisy\" mode that prints detailed"
    , "descriptions of each individual proof goal, including what type of goal it is,"
    , "why it needs to be proven, and where in the Copilot specification and/or C code"
    , "the proof goal originated from. To enable noisy mode, invoke the verifier with"
    , "the following options:"
    , ""
    , "```"
    , "verifyWithOptions (defaultVerifierOptions { verbosity = Noisy }) ..."
    , "```"
    ]
copilotLogMessageToSayWhat
    (StreamValueEqualityProofGoal verifStep goalIdx numTotalGoals
      (StreamValueEquality
        sym stateRelStep loc
        bufName offset len
        copilotTy copilotVal
        crucibleTy crucibleVal)) =
  copilotNoisily $
  displayStateRelationProofGoal
    verifStep stateRelStep VerifierProvenance goalIdx numTotalGoals
    "asserting the equality between two stream values"
    [ renderStrict $ ppProgramLoc loc
    , "* Ring buffer name: " <> bufName
    , "* Offset into buffer (from current index): " <> T.pack (show offset)
    , "* Number of elements in buffer: " <> T.pack (show len)
    , "* Copilot type: " <> T.pack (showsCopilotType 0 copilotTy "")
    , "* Copilot value:"
    , renderStrict $ PP.indent 4 $ ppCopilotValue copilotVal
    , "* Crucible value:"
    , renderStrict $ PP.indent 4 $ ppCrucibleValue sym crucibleTy crucibleVal
    ]
copilotLogMessageToSayWhat
    (TriggersInvokedCorrespondinglyProofGoal step goalIdx numTotalGoals
      (TriggersInvokedCorrespondingly loc name expected actual)) =
  copilotNoisily $
  displayProofGoal
    step VerifierProvenance goalIdx numTotalGoals
    "asserting triggers fired in corresponding ways"
    [ renderStrict $ ppProgramLoc loc
    , "* Trigger name: " <> T.pack name
    , "* Expected number of times trigger was fired:"
    , renderStrict $ PP.indent 4 $ WI.printSymNat expected
    , "* Actual number of times trigger was fired:"
    , renderStrict $ PP.indent 4 $ WI.printSymNat actual
    ]
copilotLogMessageToSayWhat
    (TriggerArgumentEqualityProofGoal step goalIdx numTotalGoals
      (TriggerArgumentEquality
        sym loc
        triggerName argNum
        copilotTy copilotVal
        crucibleTy crucibleVal)) =
  copilotNoisily $
  displayProofGoal
    step VerifierProvenance goalIdx numTotalGoals
    "asserting the equality between two trigger arguments"
    [ renderStrict $ ppProgramLoc loc
    , "* Trigger name: " <> T.pack triggerName
    , "* Number of argument: " <> T.pack (show argNum)
    , "* Copilot type: " <> T.pack (showsCopilotType 0 copilotTy "")
    , "* Copilot value:"
    , renderStrict $ PP.indent 4 $ ppCopilotValue copilotVal
    , "* Crucible value:"
    , renderStrict $ PP.indent 4 $ ppCrucibleValue sym crucibleTy crucibleVal
    ]
copilotLogMessageToSayWhat
    (RingBufferLoadProofGoal verifStep goalIdx numTotalGoals
      (RingBufferLoad
        _sym stateRelStep loc bufName offset len copilotTy _crucibleTy p)) =
  copilotNoisily $
  displayStateRelationProofGoal
    verifStep stateRelStep VerifierProvenance goalIdx numTotalGoals
    "asserting the validity of a memory load from a stream's ring buffer in C"
    [ renderStrict $ ppProgramLoc loc
    , "* Ring buffer name: " <> bufName
    , "* Offset into buffer (from current index): " <> T.pack (show offset)
    , "* Number of elements in buffer: " <> T.pack (show len)
    , "* Copilot type of buffer elements:" <> T.pack (showsCopilotType 0 copilotTy "")
    , "* Validity predicate:"
    , renderStrict $ PP.indent 4 $ WI.printSymExpr p
    ]
copilotLogMessageToSayWhat
    (RingBufferIndexLoadProofGoal verifStep goalIdx numTotalGoals
      (RingBufferIndexLoad _sym stateRelStep loc idxName p)) =
  copilotNoisily $
  displayStateRelationProofGoal
    verifStep stateRelStep VerifierProvenance goalIdx numTotalGoals
    "asserting the validity of a memory load from the index to a stream's ring buffer in C"
    [ renderStrict $ ppProgramLoc loc
    , "* Ring buffer index name: " <> idxName
    , "* Validity predicate:"
    , renderStrict $ PP.indent 4 $ WI.printSymExpr p
    ]
copilotLogMessageToSayWhat
    (PointerArgumentLoadProofGoal step goalIdx numTotalGoals
      (PointerArgumentLoad
        _sym loc copilotTy _crucibleTy p)) =
  copilotNoisily $
  displayProofGoal
    step VerifierProvenance goalIdx numTotalGoals
    "asserting the validity of a memory load from a pointer argument to a trigger"
    [ renderStrict $ ppProgramLoc loc
    , "* Copilot type: " <> T.pack (showsCopilotType 0 copilotTy "")
    , "* Validity predicate:"
    , renderStrict $ PP.indent 4 $ WI.printSymExpr p
    ]
copilotLogMessageToSayWhat
    (AccessorFunctionLoadProofGoal step goalIdx numTotalGoals
      (AccessorFunctionLoad _sym loc accessorName p)) =
  copilotNoisily $
  displayProofGoal
    step StepExecutionProvenance goalIdx numTotalGoals
    "asserting the validity of a memory load from a stream accessor function"
    [ renderStrict $ ppProgramLoc loc
    , "* Accessor function name: " <> WF.functionName accessorName
    , "* Validity predicate:"
    , renderStrict $ PP.indent 4 $ WI.printSymExpr p
    ]
copilotLogMessageToSayWhat
    (GuardFunctionLoadProofGoal step goalIdx numTotalGoals
      (GuardFunctionLoad _sym loc accessorName p)) =
  copilotNoisily $
  displayProofGoal
    step StepExecutionProvenance goalIdx numTotalGoals
    "asserting the validity of a memory load from a trigger guard function"
    [ renderStrict $ ppProgramLoc loc
    , "* Guard function name: " <> WF.functionName accessorName
    , "* Validity predicate:"
    , renderStrict $ PP.indent 4 $ WI.printSymExpr p
    ]
copilotLogMessageToSayWhat
    (UnknownFunctionLoadProofGoal step goalIdx numTotalGoals
      (UnknownFunctionLoad _sym loc accessorName p)) =
  copilotNoisily $
    displayProofGoal
    step StepExecutionProvenance goalIdx numTotalGoals
    "asserting the validity of a memory load from an unknown function"
    [ renderStrict $ ppProgramLoc loc
    , "* Function name: " <> WF.functionName accessorName
    , "* Validity predicate:"
    , renderStrict $ PP.indent 4 $ WI.printSymExpr p
    ]
copilotLogMessageToSayWhat
    (LLVMBadBehaviorCheckProofGoal step goalIdx numTotalGoals
      (LLVMBadBehaviorCheck _sym loc stk bb p)) =
  let ppLoc = renderStrict $ ppProgramLoc loc
      ppCallStackLines =
        [ "* Call stack:"
        , "    " <> renderCallStack stk
        ]
      ppValidPredLines =
        [ "* Validity predicate:"
        , renderStrict $ PP.indent 4 $ WI.printSymExpr p
        ] in
  case bb of
    LCLE.BBUndefinedBehavior ub ->
      copilotNoisily $
      displayProofGoal
        step StepExecutionProvenance goalIdx numTotalGoals
        "asserting that LLVM undefined behavior does not occur"
        $ ppLoc : ppCallStackLines ++
        [ "* Undefined behavior description:"
        , renderStrict $ PP.indent 4 $ LCLEUB.ppDetails ub
        ] ++ ppValidPredLines
    LCLE.BBMemoryError me ->
      copilotNoisily $
      displayProofGoal
        step StepExecutionProvenance goalIdx numTotalGoals
        "asserting that LLVM memory unsafety does not occur"
        $ ppLoc : ppCallStackLines ++
        [ "* Memory unsafety description:"
        , renderStrict $ PP.indent 4 $ LCLEME.explain me
        ] ++ ppValidPredLines

describeVerificationStep :: VerificationStep -> Text
describeVerificationStep InitialState     = "initial state"
describeVerificationStep StepBisimulation = "step bisimulation"

-- | Display information about an emitted proof goal.
displayProofGoal ::
     VerificationStep
  -> AssertionProvenance
  -> Integer
  -> Integer
  -> Text
  -> [Text]
  -> Text
displayProofGoal step assertProv goalIdx numTotalGoals why ls = T.unlines $
  [ banner
  , "Emitted a proof goal (" <> why <> ")"
  , "  During the " <> displayStep
  , "  Required for proving the " <> displayAssertProv
  , "  Proof goal " <> T.pack (show goalIdx)
                    <> " ("
                    <> T.pack (show numTotalGoals)
                    <> " total)"
  , ""
  ]
  ++ ls ++ [banner]
  where
    banner = "====="
    displayStep =
      case step of
        InitialState ->
          "initial bisimulation state step"
        StepBisimulation ->
          "transition step of bisimulation"
    displayAssertProv =
      case assertProv of
        VerifierProvenance ->
          "correspondence between the spec and C code"
        StepExecutionProvenance ->
          "memory safety of the generated step() function"

-- | Display information about an emitted proof goal that involves checking the
-- state relation.
displayStateRelationProofGoal ::
     VerificationStep
  -> StateRelationStep
  -> AssertionProvenance
  -> Integer
  -> Integer
  -> Text
  -> [Text]
  -> Text
displayStateRelationProofGoal verifStep stateRelStep assertProv goalIdx numTotalGoals why =
    displayProofGoal verifStep assertProv goalIdx numTotalGoals why'
  where
    why' :: Text
    why' =
      case stateRelStep of
        PreStepStateRelation  -> why <> " before calling step()"
        PostStepStateRelation -> why <> " after calling step()"
        -- `displayProofGoal` already makes a note of the fact that we're
        -- checking the initial state, so no need to do so again here.
        InitialStateRelation -> why

ppProgramLoc :: WPL.ProgramLoc -> PP.Doc a
ppProgramLoc pl = PP.vcat
  [ "* Function:" PP.<+> PP.pretty (WPL.plFunction pl)
  , "  Position:" PP.<+> PP.pretty (WPL.plSourceLoc pl)
  ]

renderCallStack :: LCLMCS.CallStack -> Text
renderCallStack cs
  | T.null ppText
  = "<no call stack available>"
  | otherwise
  = ppText
  where
    ppText = renderStrict $ LCLMCS.ppCallStack cs

showsCopilotType :: Int -> CT.Type tp -> ShowS
showsCopilotType prec tp =
  case tp of
    CT.Bool     -> showString "Bool"
    CT.Int8     -> showString "Int8"
    CT.Int16    -> showString "Int16"
    CT.Int32    -> showString "Int32"
    CT.Int64    -> showString "Int64"
    CT.Word8    -> showString "Word8"
    CT.Word16   -> showString "Word16"
    CT.Word32   -> showString "Word32"
    CT.Word64   -> showString "Word64"
    CT.Float    -> showString "Float"
    CT.Double   -> showString "Double"
    CT.Array t  -> showParen (prec >= 11) $
                     showString "Array" .
                     showsPrec 11 (CT.typeSize tp) .
                     showsCopilotType 11 t
    CT.Struct x -> showString $ CT.typeName x

ppCopilotValue :: WI.IsSymExprBuilder sym => CW4.XExpr sym -> PP.Doc a
ppCopilotValue val =
  case val of
    CW4.XBool   b -> WI.printSymExpr b
    CW4.XInt8   i -> WI.printSymExpr i
    CW4.XInt16  i -> WI.printSymExpr i
    CW4.XInt32  i -> WI.printSymExpr i
    CW4.XInt64  i -> WI.printSymExpr i
    CW4.XWord8  w -> WI.printSymExpr w
    CW4.XWord16 w -> WI.printSymExpr w
    CW4.XWord32 w -> WI.printSymExpr w
    CW4.XWord64 w -> WI.printSymExpr w
    CW4.XFloat  f -> WI.printSymExpr f
    CW4.XDouble d -> WI.printSymExpr d
    CW4.XEmptyArray {} -> "[]"
    CW4.XArray  a   -> ppBracesWith ppCopilotValue (PV.toList a)
    CW4.XStruct s   -> ppBracketsWith ppCopilotValue s

ppCrucibleValue :: WI.IsSymExprBuilder sym
                => sym
                -> LCT.TypeRepr tp
                -> LCS.RegValue sym tp
                -> PP.Doc a
ppCrucibleValue sym tp val =
  case tp of
    LCLM.LLVMPointerRepr _ -> LCLM.ppPtr val
    LCT.FloatRepr _        -> WI.printSymExpr val
    LCT.VectorRepr tpr     -> ppBracketsWith (ppCrucibleValue sym tpr) (V.toList val)
    LCT.StructRepr ctx     -> withBraces $
                              PWI.itoListFC (\i (LCS.RV v) -> ppCrucibleValue sym (ctx Ctx.! i) v) val
    _ -> error $ "ppCrucibleValue: Unsupported type: " ++ show tp

renderStrict :: PP.Doc a ->  Text
renderStrict = PP.renderStrict . PP.layoutPretty PP.defaultLayoutOptions

ppBracketsWith :: (a -> PP.Doc b) -> [a] -> PP.Doc b
ppBracketsWith f = PP.align . PP.list . map f

ppBracesWith :: (a -> PP.Doc b) -> [a] -> PP.Doc b
ppBracesWith f = withBraces . map f

withBraces :: [PP.Doc a] -> PP.Doc a
withBraces =
    PP.align
  . PP.encloseSep (PP.flatAlt "{ " "{") (PP.flatAlt " }" "}") ", "

$(deriveToJSON defaultOptions ''CopilotLogMessage)
