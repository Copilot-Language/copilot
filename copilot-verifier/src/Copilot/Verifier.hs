{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Copilot.Verifier
  ( verify
  , verifyWithOptions
  , VerifierOptions(..)
  , defaultVerifierOptions
  , sideCondVerifierOptions
  , Verbosity(..)
  ) where

import Control.Lens (view, (^.), to)
import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execStateT, lift, StateT(..))
import Data.Aeson (ToJSON)
import Data.Foldable (traverse_)
import Data.Functor (void)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Data.IORef (newIORef, modifyIORef', readIORef, IORef)
import qualified Text.LLVM.AST as L
import Data.List (genericLength, sort)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.BitVector.Sized as BV
import GHC.Generics (Generic)
import qualified Prettyprinter as PP
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>))

import Copilot.Compile.C99 (CSettings(..), compileWith)
import Copilot.Core
import qualified Copilot.Core.Type as CT

import qualified Copilot.Theorem.What4 as CW4

import qualified Copilot.Verifier.FloatMode as FloatMode
import qualified Copilot.Verifier.Log as Log
import qualified Copilot.Verifier.Solver as Solver

import Data.Parameterized.Ctx (EmptyCtx)
import Data.Parameterized.Context (pattern Empty)
import qualified Data.Parameterized.Context as Ctx
import Data.Parameterized.NatRepr (intValue, natValue, testEquality, knownNat, type (<=) )
import Data.Parameterized.Nonce (globalNonceGenerator)
import Data.Parameterized.Some (Some(..))
import Data.Parameterized.TraversableFC (toListFC)
import Data.Parameterized.TraversableFC.WithIndex (ifoldlMFC)
import qualified Data.Parameterized.Vector as PVec

import Lang.Crucible.Backend
  ( IsSymInterface, Goals(..), Assumptions, Assertion
  , pushAssumptionFrame, popUntilAssumptionFrame
  , getProofObligations, clearProofObligations
  , LabeledPred(..), abortExecBecause, AbortExecReason(..), addAssumption
  , addDurableAssertion, addDurableProofObligation
  , CrucibleAssumption(..), ppAbortExecReason
  , IsSymBackend(..), HasSymInterface(..)
  , labeledPred, labeledPredMsg
  -- , ProofObligations, proofGoal, goalsToList, labeledPredMsg
  )
import Lang.Crucible.Backend.Simple (SimpleBackend, newSimpleBackend)
import Lang.Crucible.CFG.Core (AnyCFG(..), cfgArgTypes, cfgReturnType)
import Lang.Crucible.CFG.Common ( freshGlobalVar )
import Lang.Crucible.FunctionHandle (HandleAllocator, newHandleAllocator)
import Lang.Crucible.Simulator
  ( SimContext(..), ctxSymInterface, ExecResult(..), ExecState(..)
  , defaultAbortHandler, runOverrideSim, partialValue, gpValue
  , GlobalVar, executeCrucible, OverrideSim, ovrWithBackend, regValue
  , readGlobal, modifyGlobal, callCFG, emptyRegMap, RegEntry(..)
  , AbortedResult(..)
  )
import Lang.Crucible.Simulator.ExecutionTree ( withBackend )
import Lang.Crucible.Simulator.GlobalState ( insertGlobal )
import Lang.Crucible.Simulator.RegValue (RegValue, RegValue'(..))
import Lang.Crucible.Simulator.SimError (SimError(..), SimErrorReason(..)) -- ppSimError
import Lang.Crucible.Types
  ( TypeRepr(..), (:~:)(..), KnownRepr(..), NatType )

import Lang.Crucible.LLVM (llvmGlobals, registerLazyModule, register_llvm_overrides)
import Lang.Crucible.LLVM.Bytes (bitsToBytes)
import Lang.Crucible.LLVM.DataLayout (Alignment, DataLayout)
import Lang.Crucible.LLVM.Errors (BadBehavior)
import Lang.Crucible.LLVM.Extension (LLVM, ArchWidth)
import Lang.Crucible.LLVM.Globals (initializeAllMemory, populateAllGlobals)
import Lang.Crucible.LLVM.Intrinsics
  ( IntrinsicsOptions, OverrideTemplate, basic_llvm_override, LLVMOverride(..) )

import Lang.Crucible.LLVM.MemType
  ( MemType(..), SymType(..)
  , i1, i8, i16, i32, i64
  , memTypeSize, memTypeAlign
  , mkStructInfo
  )
import Lang.Crucible.LLVM.MemModel
  ( mkMemVar, withPtrWidth, HasLLVMAnn, LLVMAnnMap, MemImpl
  , HasPtrWidth, doResolveGlobal, doStore
  , LLVMPtr, LLVMVal, MemOptions, PartLLVMVal(..), StorageType, bitvectorType
  , ptrAdd, toStorableType, projectLLVM_bv
  , pattern LLVMPointerRepr, pattern PtrRepr, loadRaw, llvmPointer_bv
  , memRepr, Mem, unpackMemValue
  )
import Lang.Crucible.LLVM.MemModel.CallStack (CallStack)
import Lang.Crucible.LLVM.MemModel.Partial (BoolAnn(..))
import Lang.Crucible.LLVM.PrettyPrint (ppSymbol)
import Lang.Crucible.LLVM.Translation
  ( LLVMTranslationWarning(..), ModuleTranslation
  , getTranslatedCFG, translateModule, globalInitMap
  , transContext, llvmPtrWidth, llvmTypeCtx, llvmTypeAsRepr
  )
import Lang.Crucible.LLVM.TypeContext (TypeContext, llvmDataLayout)

import Crux (defaultOutputConfig)
import Crux.Config (cfgJoin, Config(..))
import Crux.Config.Load (fromFile, fromEnv)
import Crux.Config.Common
  ( cruxOptions, CruxOptions(..), postprocessOptions, outputOptions
  , OutputOptions(..)
  )
import Crux.Goal (proveGoalsOffline, provedGoalsTree)
import qualified Crux.Log as Log
import Crux.Types (SimCtxt, Crux, ProcessedGoals(..), ProofResult(..))

import Crux.LLVM.Config (llvmCruxConfig, LLVMOptions(..))
import Crux.LLVM.Compile (genBitCode)
import qualified Crux.LLVM.Log as Log
import Crux.LLVM.Simulate (setupSimCtxt, parseLLVM, explainFailure)
import CruxLLVMMain (processLLVMOptions)

import What4.Config
  (extendConfig)
import What4.Interface
  ( Pred, bvLit, bvAdd, bvUrem, bvMul, bvIsNonzero, bvEq, isEq
  , getConfiguration, freshBoundedBV, predToBV
  , getCurrentProgramLoc, printSymExpr
  , truePred, falsePred, andPred, annotateTerm, backendPred
  , getAnnotation, natAdd, natEq, natIte, natLit
  )
import What4.Expr.Builder
  ( Flags, ExprBuilder, BoolExpr, startCaching
  , newExprBuilder
  )
import What4.FunctionName (functionName)
import What4.InterpretedFloatingPoint
  ( FloatInfoRepr(..), IsInterpretedFloatExprBuilder(..)
  , SingleFloat, DoubleFloat
  )
import What4.ProgramLoc (ProgramLoc, mkProgramLoc, plFunction, Position(..))
import What4.Solver.Adapter (SolverAdapter(..))
import What4.Symbol (safeSymbol)

-- | @'verify' csettings props prefix spec@ verifies the Copilot specification
-- @spec@ under the assumptions @props@ matches the behavior of the C program
-- compiled with @csettings@ within a directory prefixed by @prefix@.
verify :: CSettings -> [String] -> String -> Spec -> IO ()
verify = verifyWithOptions defaultVerifierOptions

-- | Options for configuring the behavior of the verifier.
data VerifierOptions = VerifierOptions
  { verbosity :: Verbosity
    -- ^ How much output the verifier should produce.
  , assumePartialSideConds :: Bool
    -- ^ If 'True', the verifier will determine the conditions under which
    --   a Copilot specification's partial operations are well defined and
    --   add these side conditions as assumptions. As a result, even if the
    --   generated C code performs a partial operation, the verification will
    --   succeed if this partial operation coincides with a corresponding
    --   operation on the Copilot side.
    --
    --   If 'False', the verifier will not assume any side conditions related
    --   to partial operations in the Copilot specification. As a result, any
    --   use of a partial operation in the generated C code will cause
    --   verification to fail unless the user adds their own assumptions.
  , logSmtInteractions :: Bool
    -- ^ If 'True', create log files corresponding to the SMT solver
    -- interactions used to discharge each proof goal. The file will be named
    -- @<step>-<goal number>-<solver>.smt2@, where:
    --
    -- * @<step>@ will be either @initial-step@ or @transition-step@, depending
    --   on which step of the proof the goal corresponds to.
    --
    -- * @<goal number>@ will be the number of the goal, starting at 0 and
    --   counting up. Note that each step of the proof has its own goal
    --   numbers. This means that there can be both an
    --   @initial-step-0-<solver>.smt2@ and a @transition-step-0-<solver>.smt2@,
    --   and similarly for other numbers.
    --
    -- * @<solver>@ is the name of the SMT solver used to discharge the proof
    --   goal. Currently, this will always be @z3@, although we might make this
    --   configurable in the future.
  , smtSolver :: Solver.Solver
    -- ^ Which SMT solver to use when solving proof goals.
  , smtFloatMode :: FloatMode.FloatMode
    -- ^ How the verifier should interpret floating-point operations when
    -- translating them to SMT.
    --
    -- By default, the verifier will treat all floating-point operations as
    -- uninterpreted functions ('FloatMode.FloatUninterpreted'). This allows the
    -- verifier to perform some limited reasoning about floating-point
    -- operations that SMT solvers do not have built-in operations for (@sin@,
    -- @cos@, @tan@, etc.), but at the expense of not being able to verify C
    -- code in which the compiler optimizes floating-point expressions. One can
    -- also opt into an alternative mode where floating-point values are treated
    -- as IEEE-754 floats ('FloatMode.FloatIEEE'), but this comes with the
    -- drawback that the verifier will not be able to perform /any/ reasoning
    -- about @sin@, @cos@, @tan@, etc.
  } deriving stock Show

-- | The default 'VerifierOptions':
--
-- * Produce a reasonable amount of diagnostics as verification proceeds
--   ('Default').
--
-- * Do not assume any side conditions related to partial operations.
--
-- * Do not log any SMT solver interactions.
--
-- * Use the Z3 SMT solver.
--
-- * Treat all floating-point operations as uninterpreted functions when
--   translating to SMT.
defaultVerifierOptions :: VerifierOptions
defaultVerifierOptions = VerifierOptions
  { verbosity = Default
  , assumePartialSideConds = False
  , logSmtInteractions = False
  , smtSolver = Solver.Z3
  , smtFloatMode = FloatMode.FloatUninterpreted
  }

-- | Like 'defaultVerifierOptions', except that the verifier will assume side
-- conditions related to partial operations used in the Copilot spec.
sideCondVerifierOptions :: VerifierOptions
sideCondVerifierOptions = defaultVerifierOptions
  { assumePartialSideConds = True
  }

-- | How much output should verification produce?
--
-- The data constructors are listed in increasing order of how many diagnostics
-- they produce.
data Verbosity
  = Quiet   -- ^ Don't produce any diagnostics.
  | Default -- ^ Produce a reasonable amount of diagnostics as verification proceeds.
  | Noisy   -- ^ Produce as many diagnostics as possible.
  deriving stock (Eq, Ord, Show)

-- | Like 'verify', but with 'VerifierOptions' to more finely control the
-- verifier's behavior.
verifyWithOptions :: VerifierOptions -> CSettings -> [String] -> String -> Spec -> IO ()
verifyWithOptions opts csettings0 properties prefix spec =
  withCopilotLogging $
  do -- munge options structures into the necessary forms
     (ocfg, cruxOpts, llvmOpts, csettings, csrc) <- computeConfiguration opts csettings0 prefix
     let ?outputConfig = ocfg

     -- Compile the Copilot spec into C source code, using
     -- preexisting Copilot library calls.
     compileWith csettings prefix spec
     Log.sayCopilot $ Log.GeneratedCFile csrc

     -- Compile the C source into LLVM bitcode, using preexisting
     -- Crux library calls.
     bcFile <- genBitCode cruxOpts llvmOpts
     Log.sayCopilot $ Log.CompiledBitcodeFile prefix bcFile

     -- Run the main verification procedure
     verifyBitcode opts csettings properties spec cruxOpts llvmOpts csrc bcFile


-- | Do the (surprisingly large amount) of options munging necessary to set up
--   the crucible/crux environment.
computeConfiguration ::
  Log.SupportsCruxLogMessage CopilotLogging =>
  VerifierOptions -> CSettings -> FilePath ->
  IO (Log.OutputConfig CopilotLogging, CruxOptions, LLVMOptions, CSettings, FilePath)
computeConfiguration opts csettings0 prefix =
  do ocfg1 <- defaultOutputConfig copilotLoggingToSayWhat
     let quiet = verbosity opts == Quiet
     let ocfg2 mbOutputOpts = (ocfg1 mbOutputOpts) { Log._quiet = quiet }
     llvmcfg <- llvmCruxConfig
     let cfg = cfgJoin cruxOptions llvmcfg
     -- TODO, load from an actual configuration file?
     fileOpts <- fromFile "copilot-verifier" cfg Nothing
     (cruxOpts0, llvmOpts0) <- foldM fromEnv fileOpts (cfgEnv cfg)
     let odir0 = cSettingsOutputDirectory csettings0
     let odir = -- A bit grimy, but this corresponds to how crux-llvm sets
                -- its output directory.
                if odir0 == "."
                  then "results" </> prefix
                  else odir0
     let csettings = csettings0{ cSettingsOutputDirectory = odir }
     let csrc = odir </> prefix ++ ".c"
     let cruxOpts1 = cruxOpts0{ outDir = odir, bldDir = odir, inputFiles = [csrc]
                              , outputOptions =
                                  (outputOptions cruxOpts0)
                                    { quietMode = quiet
                                    , simVerbose = if verbosity opts > Default
                                                   then 2
                                                   else 0
                                    }
                              }
     let ?outputConfig = ocfg2 (Just (outputOptions cruxOpts1))
     cruxOpts2 <- postprocessOptions cruxOpts1

     -- Tweak the options passed to Clang:
     --
     -- - Fix the optimization level to -O0.
     --
     -- - Pass -ffp-contract=off to prevent sequences of floating-point
     --   multiplications/additions from being optimized to llvm.fmuladd
     --   intrinsics, which makes floating-point verification fragile.
     let llvmOpts1 = llvmOpts0
                       { optLevel = 0
                       , clangOpts = "-ffp-contract=off" : clangOpts llvmOpts0
                       }
     (cruxOpts3, llvmOpts2) <- processLLVMOptions (cruxOpts2, llvmOpts1)

     let ocfg3 = ocfg2 (Just (outputOptions cruxOpts3))
     return (ocfg3, cruxOpts3, llvmOpts2, csettings, csrc)


data CopilotVerifierData t = CopilotVerifierData


-- | Main entry point for the verifier.
verifyBitcode ::
  Log.Logs msgs =>
  Log.SupportsCruxLogMessage msgs =>
  Log.SupportsCruxLLVMLogMessage msgs =>
  Log.SupportsCopilotLogMessage msgs =>
  VerifierOptions {- ^ Verifier-specific settings -} ->
  CSettings   {- ^ Settings used to compile the Copilot spec. Used to find the names of functions and variables. -} ->
  [String]    {- ^ Names of properties to assume during verification. -} ->
  Spec        {- ^ The input Copilot specification -} ->
  CruxOptions {- ^ Crux options -} ->
  LLVMOptions {- ^ CruxLLVM options -} ->
  FilePath    {- ^ Path to the generated C file (for logging purposes only) -} ->
  FilePath    {- ^ Path to the bitcode file to verify -} ->
  IO ()
verifyBitcode opts csettings properties spec cruxOpts llvmOpts cFile bcFile =
  FloatMode.withFloatMode (smtFloatMode opts) $ \fm ->
  do -- Set up the expression builder and symbolic backend
     halloc <- newHandleAllocator
     sym <- newExprBuilder fm CopilotVerifierData globalNonceGenerator
     bak <- newSimpleBackend sym
     -- turn on hash-consing
     startCaching sym

     FloatMode.withInterpretedFloatExprBuilder sym fm $
       verifyWithSymBackend bak halloc
  where
    verifyWithSymBackend ::
      forall t st fm.
      IsInterpretedFloatExprBuilder (ExprBuilder t st (Flags fm)) =>
      SimpleBackend t st (Flags fm) ->
      HandleAllocator ->
      IO ()
    verifyWithSymBackend bak halloc = do
      let sym = backendGetSym bak
      -- capture LLVM side-condition information for use in error reporting
      clRefs <- newCopilotLogRefs
      let ?recordLLVMAnnotation = recordLLVMAnnotation clRefs

      -- Set up the solver to use for verification.
      let adapter :: SolverAdapter st
          adapter = Solver.solverAdapter (smtSolver opts)
      extendConfig (solver_adapter_config_options adapter) (getConfiguration sym)

      -- Set up the Crucible/LLVM simulation context
      memVar <- mkMemVar "llvm_memory" halloc
      let simctx = (setupSimCtxt halloc bak (memOpts llvmOpts) memVar)
                   { printHandle = view Log.outputHandle ?outputConfig }

      -- Load and translate the input LLVM module
      llvmMod <- parseLLVM bcFile
      Some trans <-
         let ?transOpts = transOpts llvmOpts
          in translateModule halloc memVar llvmMod

      Log.sayCopilot Log.TranslatedToCrucible

      -- Grab some metadata from the bitcode file and options;
      -- make the available via implicit arguments to the places
      -- that expect them.
      let llvmCtxt = trans ^. transContext
      let ?lc = llvmCtxt ^. llvmTypeCtx
      let ?memOpts = memOpts llvmOpts
      let ?intrinsicsOpts = intrinsicsOpts llvmOpts

      llvmPtrWidth llvmCtxt $ \ptrW ->
        withPtrWidth ptrW $

        do -- Compute the LLVM memory state with global variables allocated
           -- but not initialized
           emptyMem   <- initializeAllMemory bak llvmCtxt llvmMod

           -- Compute the LLVM memory state with global variables initialized
           -- to their initial values.
           initialMem <- populateAllGlobals bak (trans ^. globalInitMap) emptyMem

           -- Use the Copilot spec directly to compute the symbolic states
           -- necessary to carry out the states of the bisimulation proof.
           Log.sayCopilot Log.GeneratingProofState
           proofStateBundle <- CW4.computeBisimulationProofBundle sym properties spec

           -- First check that the initial state of the program matches the starting
           -- segment of the associated Copilot streams.
           let cruxOptsInit = setCruxOfflineSolverOutput "initial-step" cruxOpts
           initGoals <-
             verifyInitialState cruxOptsInit [adapter] clRefs simctx initialMem
               (CW4.initialStreamState proofStateBundle)

           -- Now, the real meat. Carry out the bisimulation step of the proof.
           let cruxOptsTrans = setCruxOfflineSolverOutput "transition-step" cruxOpts
           bisimGoals <-
             verifyStepBisimulation opts cruxOptsTrans [adapter] csettings
               clRefs simctx llvmMod trans memVar initialMem proofStateBundle

           Log.sayCopilot $ Log.SuccessfulProofSummary cFile initGoals bisimGoals
           -- We only want to inform users about Noisy if the verbosity level is
           -- sufficiently low. Crux's logging framework isn't particularly
           -- suited to doing this, as it assumes that all log messages enabled
           -- for low verbosity levels should also be enabled for higher
           -- verbosity levels. That is a reasonable assumption most of the time,
           -- but not here.
           --
           -- To compensate, we hack around the issue by manually checking the
           -- verbosity level before logging the message.
           when (verbosity opts < Noisy) $
             Log.sayCopilot Log.NoisyVerbositySuggestion

    -- If @logSmtInteractions@ is enabled, enable offline solver output in the
    -- supplied 'CruxOptions' with the supplied file template. Otherwise, return
    -- the supplied 'CruxOptions' unaltered.
    setCruxOfflineSolverOutput :: FilePath -> CruxOptions -> CruxOptions
    setCruxOfflineSolverOutput template cruxOpts'
      | logSmtInteractions opts
      = cruxOpts'
          { offlineSolverOutput = Just $ outDir cruxOpts' </> template <.> "smt2" }
      | otherwise
      = cruxOpts'

-- | Capture LLVM side-condition information for use in error reporting.
recordLLVMAnnotation ::
  IsSymInterface sym =>
  CopilotLogRefs sym ->
  CallStack ->
  BoolAnn sym ->
  BadBehavior sym ->
  IO ()
recordLLVMAnnotation clRefs stk bann bb =
  modifyIORef' (llvmAnnMapRef clRefs) (Map.insert bann (stk, bb))

-- | Prove that the state of the global variables at program startup
--   matches the expected initial segments of the associated Copilot
--   streams.
verifyInitialState ::
  IsSymInterface sym =>
  Log.Logs msgs =>
  Log.SupportsCruxLogMessage msgs =>
  Log.SupportsCopilotLogMessage msgs =>
  sym ~ ExprBuilder t st fs =>
  HasPtrWidth wptr =>
  HasLLVMAnn sym =>
  (?memOpts :: MemOptions) =>
  (?lc :: TypeContext) =>

  CruxOptions ->
  [SolverAdapter st] ->
  CopilotLogRefs sym ->
  SimCtxt Crux sym LLVM ->
  MemImpl sym ->
  CW4.BisimulationProofState sym ->
  IO Integer
verifyInitialState cruxOpts adapters clRefs simctx mem initialState =
  withBackend simctx $ \bak ->
  do Log.sayCopilot $ Log.ComputingConditions Log.InitialState
     frm <- pushAssumptionFrame bak

     assertStateRelation bak clRefs mem
       Log.InitialStateRelation initialState

     popUntilAssumptionFrame bak frm

     Log.sayCopilot $ Log.ProvingConditions Log.InitialState
     proveObls cruxOpts adapters clRefs Log.InitialState simctx


verifyStepBisimulation ::
  IsSymInterface sym =>
  Log.Logs msgs =>
  Log.SupportsCruxLogMessage msgs =>
  Log.SupportsCruxLLVMLogMessage msgs =>
  Log.SupportsCopilotLogMessage msgs =>
  sym ~ ExprBuilder t st fs =>
  HasPtrWidth wptr =>
  HasLLVMAnn sym =>
  (1 <= ArchWidth arch) =>
  HasPtrWidth (ArchWidth arch) =>
  (?memOpts :: MemOptions) =>
  (?lc :: TypeContext) =>
  (?intrinsicsOpts :: IntrinsicsOptions) =>

  VerifierOptions ->
  CruxOptions ->
  [SolverAdapter st] ->
  CSettings ->
  CopilotLogRefs sym ->
  SimCtxt Crux sym LLVM ->
  L.Module ->
  ModuleTranslation arch ->
  GlobalVar Mem ->
  MemImpl sym ->
  CW4.BisimulationProofBundle sym ->
  IO Integer
verifyStepBisimulation opts cruxOpts adapters csettings clRefs simctx llvmMod modTrans memVar mem prfbundle =
  withBackend simctx $ \bak ->
  do Log.sayCopilot $ Log.ComputingConditions Log.StepBisimulation

     frm <- pushAssumptionFrame bak

     do -- set up the memory image
        mem' <- setupPrestate bak mem prfbundle

        -- sanity check, verify that we set up the memory in the expected relation
        assertStateRelation bak clRefs mem'
          Log.PreStepStateRelation (CW4.preStreamState prfbundle)

        -- set up trigger guard global variables
        let halloc = simHandleAllocator simctx
        -- See Note [Global variables for trigger functions]
        let prepTrigger (nm, guard, _) =
              do gv <- freshGlobalVar halloc (Text.pack (nm ++ "_called")) NatRepr
                 return (nm, gv, guard)

            checkDuplicateTriggerNames :: [Name] -> IO ()
            checkDuplicateTriggerNames triggers =
              traverse_ checkDuplicateTriggerName $ NE.group $ sort triggers

            checkDuplicateTriggerName :: NonEmpty Name -> IO ()
            checkDuplicateTriggerName (trig :| dupTrigs) =
              unless (null dupTrigs) $
                fail $ unlines
                  [ "The specification invokes the `" ++ trig ++
                    "` trigger function multiple times,"
                  , "which copilot-verifier does not currently support."
                  , "See https://github.com/Copilot-Language/copilot-verifier/issues/74."
                  ]
        let triggerState = CW4.triggerState prfbundle
        checkDuplicateTriggerNames $ map (\(nm,_,_) -> nm) triggerState
        triggerGlobals <- mapM prepTrigger triggerState

        -- execute the step function
        let overrides = zipWith (triggerOverride clRefs) triggerGlobals triggerState
        mem'' <- executeStep opts csettings clRefs simctx memVar mem' llvmMod modTrans triggerGlobals overrides (CW4.assumptions prfbundle) (CW4.sideConds prfbundle)

        -- assert the poststate is in the relation
        assertStateRelation bak clRefs mem''
          Log.PostStepStateRelation (CW4.postStreamState prfbundle)

     popUntilAssumptionFrame bak frm

     Log.sayCopilot $ Log.ProvingConditions Log.StepBisimulation
     proveObls cruxOpts adapters clRefs Log.StepBisimulation simctx


-- | Set up the "trigger override" functions.  These dummy functions
--   take the place of the external functions called by the Copilot
--   monitor when a guarded condition occurs.
--
--   Each trigger statement has a corresponding global variable that
--   is used to record if the trigger function was called; initially
--   the global is false, and is set to true when the trigger function
--   is called.  At the end of verification, we check that the value
--   of this global variable is true iff the corresponding trigger guard
--   condition is true.
--
--   The other function of the trigger overrides is to check that, when called,
--   the functions are given the expected argument values.
--
--   Otherwise, the override functions have no effects, which corresponds
--   to the assumption that the external environment makes no changes to the
--   program state that are observable to the Copilot monitor.
triggerOverride :: forall sym t arch msgs.
  IsSymInterface sym =>
  Log.Logs msgs =>
  Log.SupportsCopilotLogMessage msgs =>
  (?memOpts :: MemOptions) =>
  (?lc :: TypeContext) =>
  (?intrinsicsOpts :: IntrinsicsOptions) =>
  (1 <= ArchWidth arch) =>
  HasPtrWidth (ArchWidth arch) =>
  HasLLVMAnn sym =>

  CopilotLogRefs sym ->
  (Name, GlobalVar NatType, Pred sym) ->
  (Name, BoolExpr t, [(Some Type, CW4.XExpr sym)]) ->
  OverrideTemplate (Crux sym) sym LLVM arch
triggerOverride clRefs (_,triggerGlobal,_) (nm, _guard, args) =
   let args' = map toTypeRepr args in
   case Ctx.fromList args' of
     Some argCtx ->
      basic_llvm_override $
      LLVMOverride decl argCtx UnitRepr $
        \memOps calledArgs ->
          ovrWithBackend $ \bak ->
          do let sym = backendGetSym bak
             modifyGlobal triggerGlobal $ \count -> do
               -- See Note [Global variables for trigger functions]
               countPlusOne <- liftIO $ do
                 one <- natLit sym 1
                 natAdd sym count one
               pure ((), countPlusOne)
             mem <- readGlobal memOps
             liftIO $ checkArgs bak mem (toListFC Some calledArgs) args
             return ()

 where
  decl = L.Declare
         { L.decLinkage = Nothing
         , L.decVisibility = Nothing
         , L.decRetType = L.PrimType L.Void
         , L.decName = L.Symbol nm
         , L.decArgs = map llvmArgTy args
         , L.decVarArgs = False
         , L.decAttrs = []
         , L.decComdat = Nothing
         }

  -- Use the `-CompositePtr` functions here to ensure that arguments with array
  -- or struct types are treated as pointers. See Note [Arrays and structs].
  toTypeRepr (Some ctp, _) = llvmTypeAsRepr (copilotTypeToMemTypeCompositePtr (llvmDataLayout ?lc) ctp) Some
  llvmArgTy (Some ctp, _) = copilotTypeToLLVMTypeCompositePtr ctp

  checkArgs :: forall bak. IsSymBackend sym bak =>
    bak -> MemImpl sym -> [Some (RegEntry sym)] -> [(Some Type, CW4.XExpr sym)] -> IO ()
  checkArgs bak mem = loop (0::Integer)
    where
    loop i (x:xs) ((ctp,v):vs) = checkArg bak mem i x ctp v >> loop (i+1) xs vs
    loop _ [] [] = return ()
    loop _ _ _ = fail $ "Argument list mismatch in " ++ nm

  checkArg :: forall bak. IsSymBackend sym bak =>
    bak -> MemImpl sym -> Integer -> Some (RegEntry sym) -> Some Type -> CW4.XExpr sym -> IO ()
  checkArg bak mem i (Some (RegEntry tp v)) (Some ctp) x =
    do let sym = backendGetSym bak
       eq <- computeEqualVals bak clRefs mem ctp x tp v
       (ann, eq') <- annotateTerm sym eq
       let shortmsg = "Trigger " ++ show nm ++ " argument " ++ show i
       let longmsg  = show (printSymExpr eq')
       let rsn      = AssertFailureSimError shortmsg longmsg
       loc <- getCurrentProgramLoc sym
       modifyIORef' (verifierAssertionMapRef clRefs)
         $ Map.insert (BoolAnn ann)
         $ Log.TriggerArgumentEqualityAssertion
         $ Log.SomeSome
         $ Log.TriggerArgumentEquality sym loc nm i ctp x tp v
       addDurableProofObligation bak (LabeledPred eq' (SimError loc rsn))


-- | Actually execute the Crucible simulator on the generated "step" function.
--   This will record proof side-conditions into the symbolic backend, and
--   return the memory state corresponding to the function post-state.
--
--   This function will record side-conditions that arise from the semantics
--   of C itself (e.g., memory is accessed in bounds and signed arithmetic
--   doesn't overflow) as well as the conditions related to trigger functions.
executeStep :: forall sym arch msgs.
  IsSymInterface sym =>
  Log.Logs msgs =>
  Log.SupportsCruxLLVMLogMessage msgs =>
  Log.SupportsCopilotLogMessage msgs =>
  (?memOpts :: MemOptions) =>
  (?lc :: TypeContext) =>
  (?intrinsicsOpts :: IntrinsicsOptions) =>
  (1 <= ArchWidth arch) =>
  HasPtrWidth (ArchWidth arch) =>
  HasLLVMAnn sym =>

  VerifierOptions ->
  CSettings ->
  CopilotLogRefs sym ->
  SimCtxt Crux sym LLVM ->
  GlobalVar Mem ->
  MemImpl sym ->
  L.Module ->
  ModuleTranslation arch ->
  [(Name, GlobalVar NatType, Pred sym)] ->
  [OverrideTemplate (Crux sym) sym LLVM arch] ->
  [Pred sym] {- User-provided property assumptions -} ->
  [Pred sym] {- Side conditions related to partial operations -} ->
  IO (MemImpl sym)
executeStep opts csettings clRefs simctx memVar mem llvmmod modTrans triggerGlobals triggerOverrides assums sideConds =
  do globSt <- foldM setupTrigger (llvmGlobals memVar mem) triggerGlobals
     let initSt = InitialState simctx globSt defaultAbortHandler memRepr $
                    runOverrideSim memRepr runStep
     res <- executeCrucible [] initSt
     case res of
       FinishedResult _ pr -> return (pr^.partialValue.gpValue.to regValue)
       AbortedResult _ abortRes -> fail $ show $ ppAbortedResult abortRes
       TimeoutResult{} -> fail "simulation timed out!"

 where
  -- See Note [Global variables for trigger functions]
  setupTrigger gs (_,gv,_) = do
    zero <- liftIO $ natLit sym 0
    pure $ insertGlobal gv zero gs
  llvm_ctx = modTrans ^. transContext
  stepName = cSettingsStepFunctionName csettings
  sym = simctx^.ctxSymInterface

  -- TODO, would be lovely to be able to do better than dummy positions for all these things
  -- so we can correspond assumptions and asserts back to the parts of the original spec that
  -- gave rise to them.
  dummyLoc = mkProgramLoc "<>" InternalPos

  assumeProperty b =
    withBackend simctx $ \bak ->
      addAssumption bak (GenericAssumption dummyLoc "Property assumption" b)

  assumeSideCond b =
    withBackend simctx $ \bak ->
      addAssumption bak (GenericAssumption dummyLoc "Side condition for partial operation" b)

  ppAbortedResult :: AbortedResult sym ext -> PP.Doc ann
  ppAbortedResult abortRes =
    case gatherReasons abortRes of
      reason :| [] -> reason
      reasons      -> PP.vcat $ "Simulation aborted for multiple reasons."
                              : NE.toList reasons

  gatherReasons :: AbortedResult sym ext -> NonEmpty (PP.Doc ann)
  gatherReasons (AbortedExec rsn _) =
    PP.vcat ["Simulation aborted!", ppAbortExecReason rsn] :| []
  gatherReasons (AbortedExit ec) =
    PP.vcat ["Simulation called exit!", PP.viaShow ec] :| []
  gatherReasons (AbortedBranch _ _ t f) =
    gatherReasons t <> gatherReasons f

  -- Simulator entry point
  runStep :: OverrideSim (Crux sym) sym LLVM (RegEntry sym Mem) EmptyCtx Mem (MemImpl sym)
  runStep =
    do -- set up built-in functions and trigger overrides
       _ <- register_llvm_overrides llvmmod [] triggerOverrides llvm_ctx
       -- set up functions defined in the module
       registerLazyModule sayTranslationWarning modTrans

       -- make any property assumptions
       liftIO (mapM_ assumeProperty assums)

       -- assume side conditions related to partial operations
       when (assumePartialSideConds opts) $ liftIO $
         mapM_ assumeSideCond sideConds

       -- look up and call the step function
       mbCfg <- liftIO $ getTranslatedCFG modTrans (L.Symbol stepName)
       () <- case mbCfg of
         Just (_, AnyCFG anyCfg, warns) -> do
           liftIO $ mapM_ sayTranslationWarning warns
           case (cfgArgTypes anyCfg, cfgReturnType anyCfg) of
             (Empty, UnitRepr) -> regValue <$> callCFG anyCfg emptyRegMap
             _ -> fail $ unwords [show stepName, "should take no arguments and return void"]
         Nothing -> fail $ unwords ["Could not find step function named", show stepName]

       -- Assert that the trigger functions were called exactly once iff the
       -- associated guard condition was true.
       -- See Note [Global variables for trigger functions].
       forM_ triggerGlobals $ \(nm, gv, guard) ->
         do expectedCount <- liftIO $ do
              one  <- natLit sym 1
              zero <- natLit sym 0
              natIte sym guard one zero
            actualCount <- readGlobal gv
            eq <- liftIO $ natEq sym expectedCount actualCount
            (ann, eq') <- liftIO $ annotateTerm sym eq
            let shortmsg = "Trigger guard equality condition: " ++ show nm
            let longmsg  = show (printSymExpr eq')
            let rsn      = AssertFailureSimError shortmsg longmsg
            liftIO
              $ modifyIORef' (verifierAssertionMapRef clRefs)
              $ Map.insert (BoolAnn ann)
              $ Log.TriggersInvokedCorrespondinglyAssertion
              $ Log.TriggersInvokedCorrespondingly dummyLoc nm expectedCount actualCount
            withBackend simctx $ \bak ->
              liftIO $ addDurableProofObligation bak (LabeledPred eq' (SimError dummyLoc rsn))

       -- return the final state of the memory
       readGlobal memVar

-- | Given a bisimulation proof bundle and an empty initial state,
--   populate the global ring-buffer variables with abstract state
--   values, and write the abstract values of the external stream
--   values into their proper locations.
setupPrestate ::
  IsSymBackend sym bak =>
  HasPtrWidth wptr =>
  HasLLVMAnn sym =>
  (?memOpts :: MemOptions) =>
  (?lc :: TypeContext) =>

  bak ->
  MemImpl sym ->
  CW4.BisimulationProofBundle sym ->
  IO (MemImpl sym)
setupPrestate bak mem0 prfbundle =
  do mem' <- foldM setupStreamState mem0 (CW4.streamState (CW4.preStreamState prfbundle))
     foldM setupExternalInput mem' (CW4.externalInputs prfbundle)

 where
   sym = backendGetSym bak

   sizeTStorage :: StorageType
   sizeTStorage = bitvectorType (bitsToBytes (intValue ?ptrWidth))

   setupExternalInput mem (nm, Some ctp, v) =
     do -- Compute LLVM/Crucible type information from the Copilot type
        let memTy      = copilotTypeToMemTypeBool8 (llvmDataLayout ?lc) ctp
        let typeAlign  = memTypeAlign (llvmDataLayout ?lc) memTy
        stType <- toStorableType memTy
        Some typeRepr <- return (llvmTypeAsRepr memTy Some)

        -- resolve the global variable to a pointers
        ptrVal <- doResolveGlobal bak mem (L.Symbol nm)

        -- write the value into the global
        regVal <- copilotExprToRegValue sym v typeRepr
        doStore bak mem ptrVal typeRepr stType typeAlign regVal

   setupStreamState mem (nm, Some ctp, vs) =
     do -- TODO, should get these from somewhere inside copilot instead of building these names directly
        let idxName = "s" ++ show nm ++ "_idx"
        let bufName = "s" ++ show nm
        let buflen  = genericLength vs :: Integer

        -- Compute LLVM/Crucible type information from the Copilot type
        let memTy      = copilotTypeToMemTypeBool8 (llvmDataLayout ?lc) ctp
        let typeLen    = memTypeSize (llvmDataLayout ?lc) memTy
        let typeAlign  = memTypeAlign (llvmDataLayout ?lc) memTy
        stType <- toStorableType memTy
        Some typeRepr <- return (llvmTypeAsRepr memTy Some)

        -- Resolve the global names into base pointers
        idxPtr <- doResolveGlobal bak mem (L.Symbol idxName)
        bufPtr <- doResolveGlobal bak mem (L.Symbol bufName)

        -- Create a fresh index value in the proper range
        idxVal <- freshBoundedBV sym (safeSymbol idxName) ?ptrWidth
                     (Just 0) (Just (fromIntegral (buflen - 1)))
        idxVal' <- llvmPointer_bv sym idxVal

        -- store the index value in the correct location
        let sizeTAlign = memTypeAlign (llvmDataLayout ?lc) (IntType (natValue ?ptrWidth))
        mem' <- doStore bak mem idxPtr (LLVMPointerRepr ?ptrWidth) sizeTStorage sizeTAlign idxVal'

        buflen'  <- bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth buflen)
        typeLen' <- bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth (toInteger typeLen))

        -- Write each value of the stream ring buffer into its correct location
        flip execStateT mem' $
          forM_ (zip vs [0 ..]) $ \(v,i) ->
            do ptrVal <- lift $
                 do x1 <- bvAdd sym idxVal =<< bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth i)
                    x2 <- bvUrem sym x1 buflen'
                    x3 <- bvMul sym x2 typeLen'
                    ptrAdd sym ?ptrWidth bufPtr x3

               regVal <- lift $ copilotExprToRegValue sym v typeRepr
               StateT $ \m ->
                 do m' <- doStore bak m ptrVal typeRepr stType typeAlign regVal
                    return ((),m')

-- | Given a memory image and a "proof state", assert that the global values
--   for each stream ring buffer have values that correspond to the given
--   stream state. This collection of assertions defines the bisimulation
--   relation.
assertStateRelation ::
  IsSymBackend sym bak =>
  Log.Logs msgs =>
  HasPtrWidth wptr =>
  HasLLVMAnn sym =>
  (?memOpts :: MemOptions) =>
  (?lc :: TypeContext) =>
  bak ->
  CopilotLogRefs sym ->
  MemImpl sym ->
  Log.StateRelationStep ->
  CW4.BisimulationProofState sym ->
  IO ()
assertStateRelation bak clRefs mem stateRelStep prfst =
  -- For each stream in the proof state, assert that the
  -- generated ring buffer global contains the corresponding
  -- values.
  forM_ (CW4.streamState prfst) assertStreamState

 where
   sym = backendGetSym bak

   sizeTStorage :: StorageType
   sizeTStorage = bitvectorType (bitsToBytes (intValue ?ptrWidth))

   assertStreamState (nm, Some ctp, vs) =
     do -- TODO, should get these from somewhere inside copilot instead of building these names directly
        let idxName = "s" ++ show nm ++ "_idx"
        let bufName = "s" ++ show nm
        let buflen  = genericLength vs :: Integer

        -- Compute LLVM/Crucible type information from the Copilot type
        let memTy      = copilotTypeToMemTypeBool8 (llvmDataLayout ?lc) ctp
        let typeLen    = memTypeSize (llvmDataLayout ?lc) memTy
        let typeAlign  = memTypeAlign (llvmDataLayout ?lc) memTy
        stType <- toStorableType memTy
        Some typeRepr <- return (llvmTypeAsRepr memTy Some)

        -- Resolve the global names into base pointers
        idxPtr <- doResolveGlobal bak mem (L.Symbol idxName)
        bufPtr <- doResolveGlobal bak mem (L.Symbol bufName)

        -- read the value of the ring buffer index
        let sizeTAlign = memTypeAlign (llvmDataLayout ?lc) (IntType (natValue ?ptrWidth))
        (bannIdxVal, pIdxVal, idxVal) <-
          doLoadWithAnn bak mem idxPtr sizeTStorage (LLVMPointerRepr ?ptrWidth) sizeTAlign
        idxVal' <- projectLLVM_bv bak idxVal
        locIdxVal <- getCurrentProgramLoc sym
        modifyIORef' (verifierAssertionMapRef clRefs)
          $ Map.insert bannIdxVal
          $ Log.RingBufferIndexLoadAssertion
          $ Log.RingBufferIndexLoad sym stateRelStep locIdxVal (Text.pack idxName) pIdxVal

        buflen'  <- bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth buflen)
        typeLen' <- bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth (toInteger typeLen))

        -- For each value in the stream description, read a corresponding value from
        -- memory and assert that they are equal.
        forM_ (zip vs [0 ..]) $ \(v,i) ->
          do ptrVal <-
               do x1 <- bvAdd sym idxVal' =<< bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth i)
                  x2 <- bvUrem sym x1 buflen'
                  x3 <- bvMul sym x2 typeLen'
                  ptrAdd sym ?ptrWidth bufPtr x3

             (bannv', pv', v') <- doLoadWithAnn bak mem ptrVal stType typeRepr typeAlign
             locv' <- getCurrentProgramLoc sym
             let bufNameT = Text.pack bufName
             modifyIORef' (verifierAssertionMapRef clRefs)
               $ Map.insert bannv'
               $ Log.RingBufferLoadAssertion
               $ Log.SomeSome
               $ Log.RingBufferLoad sym stateRelStep locv' bufNameT i buflen ctp typeRepr pv'
             eq <- computeEqualVals bak clRefs mem ctp v typeRepr v'
             (ann, eq') <- annotateTerm sym eq
             let shortmsg = "State equality condition: " ++ show nm ++ " index value " ++ show i
             let longmsg  = show (printSymExpr eq')
             let rsn      = AssertFailureSimError shortmsg longmsg
             let loc      = mkProgramLoc "<>" InternalPos
             modifyIORef' (verifierAssertionMapRef clRefs)
               $ Map.insert (BoolAnn ann)
               $ Log.StreamValueEqualityAssertion
               $ Log.SomeSome
               $ Log.StreamValueEquality sym stateRelStep loc bufNameT i buflen ctp v typeRepr v'
             addDurableProofObligation bak (LabeledPred eq' (SimError loc rsn))

        return ()

-- | Translate the @XExpr@ values from the "Copilot.Theorem.What4" module into
--   Crucible @RegValue@s suitable for injection into the Crucible simulator.
copilotExprToRegValue :: forall sym tp.
  IsSymInterface sym =>
  sym ->
  CW4.XExpr sym ->
  TypeRepr tp ->
  IO (RegValue sym tp)
copilotExprToRegValue sym = loop
  where
    loop :: forall tp'. CW4.XExpr sym -> TypeRepr tp' -> IO (RegValue sym tp')

    loop (CW4.XBool b) (LLVMPointerRepr w) | Just Refl <- testEquality w (knownNat @1) =
      llvmPointer_bv sym =<< predToBV sym b knownRepr
    loop (CW4.XBool b) (LLVMPointerRepr w) | Just Refl <- testEquality w (knownNat @8) =
      llvmPointer_bv sym =<< predToBV sym b knownRepr
    loop (CW4.XInt8 x) (LLVMPointerRepr w) | Just Refl <- testEquality w (knownNat @8) =
      llvmPointer_bv sym x
    loop (CW4.XInt16 x) (LLVMPointerRepr w) | Just Refl <- testEquality w (knownNat @16) =
      llvmPointer_bv sym x
    loop (CW4.XInt32 x) (LLVMPointerRepr w) | Just Refl <- testEquality w (knownNat @32) =
      llvmPointer_bv sym x
    loop (CW4.XInt64 x) (LLVMPointerRepr w) | Just Refl <- testEquality w (knownNat @64) =
      llvmPointer_bv sym x
    loop (CW4.XWord8 x) (LLVMPointerRepr w) | Just Refl <- testEquality w (knownNat @8) =
      llvmPointer_bv sym x
    loop (CW4.XWord16 x) (LLVMPointerRepr w) | Just Refl <- testEquality w (knownNat @16) =
      llvmPointer_bv sym x
    loop (CW4.XWord32 x) (LLVMPointerRepr w) | Just Refl <- testEquality w (knownNat @32) =
      llvmPointer_bv sym x
    loop (CW4.XWord64 x) (LLVMPointerRepr w) | Just Refl <- testEquality w (knownNat @64) =
      llvmPointer_bv sym x

    loop (CW4.XFloat x)  (FloatRepr SingleFloatRepr) = return x
    loop (CW4.XDouble x) (FloatRepr DoubleFloatRepr) = return x

    loop (CW4.XEmptyArray _ctp) (VectorRepr _tpr) =
      pure V.empty
    loop (CW4.XArray xs) (VectorRepr tpr) =
      V.generateM (PVec.lengthInt xs) (\i -> loop (PVec.elemAtUnsafe i xs) tpr)
    loop (CW4.XStruct xs) (StructRepr ctx) =
      Ctx.traverseWithIndex
        (\i tpr -> RV <$> loop (xs !! Ctx.indexVal i) tpr)
        ctx

    loop x tpr =
      fail $ unlines ["Mismatch between Copilot value and crucible value", show x, show tpr]


-- | Given an @XExpr@ from from the "Copilot.Theorem.What4" module, and
--   a Crucible @RegValue@ which is expected to match, compute an equality
--   predicate between the values.  The Crucible values may be pointers,
--   requiring us to resolve the indirection through memory; this is necessary
--   for array and struct values, but would also work for scalars.
computeEqualVals :: forall sym bak tp a wptr.
  IsSymBackend sym bak =>
  HasPtrWidth wptr =>
  HasLLVMAnn sym =>
  (?lc :: TypeContext) =>
  (?memOpts :: MemOptions) =>
  bak ->
  CopilotLogRefs sym ->
  MemImpl sym ->
  Type a ->
  CW4.XExpr sym ->
  TypeRepr tp ->
  RegValue sym tp ->
  IO (Pred sym)
computeEqualVals bak clRefs mem = loop
  where
    sym = backendGetSym bak

    loop :: forall tp' a'. Type a' -> CW4.XExpr sym -> TypeRepr tp' -> RegValue sym tp' -> IO (Pred sym)
    loop Bool (CW4.XBool b) (LLVMPointerRepr w) v | Just Refl <- testEquality w (knownNat @1) =
      isEq sym b =<< bvIsNonzero sym =<< projectLLVM_bv bak v
    loop Bool (CW4.XBool b) (LLVMPointerRepr w) v | Just Refl <- testEquality w (knownNat @8) =
      isEq sym b =<< bvIsNonzero sym =<< projectLLVM_bv bak v
    loop Int8 (CW4.XInt8 x) (LLVMPointerRepr w) v | Just Refl <- testEquality w (knownNat @8) =
      bvEq sym x =<< projectLLVM_bv bak v
    loop Int16 (CW4.XInt16 x) (LLVMPointerRepr w) v | Just Refl <- testEquality w (knownNat @16) =
      bvEq sym x =<< projectLLVM_bv bak v
    loop Int32 (CW4.XInt32 x) (LLVMPointerRepr w) v | Just Refl <- testEquality w (knownNat @32) =
      bvEq sym x =<< projectLLVM_bv bak v
    loop Int64 (CW4.XInt64 x) (LLVMPointerRepr w) v | Just Refl <- testEquality w (knownNat @64) =
      bvEq sym x =<< projectLLVM_bv bak v
    loop Word8 (CW4.XWord8 x) (LLVMPointerRepr w) v | Just Refl <- testEquality w (knownNat @8) =
      bvEq sym x =<< projectLLVM_bv bak v
    loop Word16 (CW4.XWord16 x) (LLVMPointerRepr w) v | Just Refl <- testEquality w (knownNat @16) =
      bvEq sym x =<< projectLLVM_bv bak v
    loop Word32 (CW4.XWord32 x) (LLVMPointerRepr w) v | Just Refl <- testEquality w (knownNat @32) =
      bvEq sym x =<< projectLLVM_bv bak v
    loop Word64 (CW4.XWord64 x) (LLVMPointerRepr w) v | Just Refl <- testEquality w (knownNat @64) =
      bvEq sym x =<< projectLLVM_bv bak v

    loop Float (CW4.XFloat x)  (FloatRepr SingleFloatRepr) v = iFloatEq @_ @SingleFloat sym x v
    loop Double (CW4.XDouble x) (FloatRepr DoubleFloatRepr) v = iFloatEq @_ @DoubleFloat sym x v

    loop (Array _ctp) (CW4.XEmptyArray _ctp2) (VectorRepr _tpr) vs =
      pure $ backendPred sym $ V.null vs
    loop (Array ctp) (CW4.XArray xs) (VectorRepr tpr) vs
      | PVec.lengthInt xs == V.length vs
      = V.ifoldM (\pAcc i v -> andPred sym pAcc =<< loop ctp (PVec.elemAtUnsafe i xs) tpr v)
                 (truePred sym) vs
      | otherwise
      = pure (falsePred sym)
    loop (Struct struct) (CW4.XStruct xs) (StructRepr ctx) vs
      | length copilotVals == Ctx.sizeInt (Ctx.size vs)
      = ifoldlMFC (\i pAcc tpr ->
                    case copilotVals !! Ctx.indexVal i of
                      (Value ctp _, x) ->
                        andPred sym pAcc =<< loop ctp x tpr (unRV (vs Ctx.! i)))
                  (truePred sym) ctx
      | otherwise
      = pure (falsePred sym)
      where
        copilotVals :: [(Value a', CW4.XExpr sym)]
        copilotVals = zip (toValues struct) xs

    -- If we encounter a pointer, read the memory that it points to and recurse,
    -- using the Copilot type as a guide for how much memory to read. This is
    -- needed to make array- or struct-typed arguments work (see
    -- Note [Arrays and structs]), although there is nothing about this code
    -- that is array- or struct-specific. In fact, this code could also work
    -- for pointer arguments of any other type.
    loop ctp x PtrRepr v =
      do let memTy = copilotTypeToMemTypeBool8 (llvmDataLayout ?lc) ctp
             typeAlign = memTypeAlign (llvmDataLayout ?lc) memTy
         stp <- toStorableType memTy
         llvmTypeAsRepr memTy $ \tpr ->
           do loc <- getCurrentProgramLoc sym
              (bann, p, regVal) <- doLoadWithAnn bak mem v stp tpr typeAlign
              modifyIORef' (verifierAssertionMapRef clRefs)
                $ Map.insert bann
                $ Log.PointerArgumentLoadAssertion
                $ Log.SomeSome
                $ Log.PointerArgumentLoad sym loc ctp tpr p
              loop ctp x tpr regVal

    loop _ctp x tpr _v =
      fail $ unlines ["Mismatch between Copilot value and crucible value", show x, show tpr]

-- | Convert a Copilot 'CT.Type' to a Crucible 'MemType'. 'CT.Bool's are
-- assumed to be one bit in size. See @Note [How LLVM represents bool]@.
copilotTypeToMemType ::
  DataLayout ->
  CT.Type a ->
  MemType
copilotTypeToMemType dl = loop
  where
    loop :: forall t. CT.Type t -> MemType
    loop CT.Bool   = i1
    loop CT.Int8   = i8
    loop CT.Int16  = i16
    loop CT.Int32  = i32
    loop CT.Int64  = i64
    loop CT.Word8  = i8
    loop CT.Word16 = i16
    loop CT.Word32 = i32
    loop CT.Word64 = i64
    loop CT.Float  = FloatType
    loop CT.Double = DoubleType
    loop t0@(CT.Array tp) =
      let len = fromIntegral (typeLength t0) in
      ArrayType len (copilotTypeToMemTypeBool8 dl tp)
    loop (CT.Struct v) =
      StructType (mkStructInfo dl False (map val (CT.toValues v)))

    val :: forall t. CT.Value t -> MemType
    val (CT.Value tp _) = copilotTypeToMemTypeBool8 dl tp

-- | Like 'copilotTypeToMemType', except that 'CT.Bool's are assumed to be
-- eight bits, not one bit. See @Note [How LLVM represents bool]@.
copilotTypeToMemTypeBool8 ::
  DataLayout ->
  CT.Type a ->
  MemType
copilotTypeToMemTypeBool8 _dl CT.Bool = i8
copilotTypeToMemTypeBool8 dl tp = copilotTypeToMemType dl tp

-- | Like 'copilotTypeToMemType', except that composite types (i.e.,
-- 'CT.Array's and 'CT.Struct's) are converted to 'PtrType's instead of direct
-- 'ArrayType's or 'StructType's. See @Note [Arrays and structs]@.
copilotTypeToMemTypeCompositePtr ::
  DataLayout ->
  CT.Type a ->
  MemType
copilotTypeToMemTypeCompositePtr dl (CT.Array tp) =
  PtrType (MemType (copilotTypeToMemTypeBool8 dl tp))
copilotTypeToMemTypeCompositePtr _dl (CT.Struct struct) =
  PtrType (Alias (copilotStructIdent struct))
copilotTypeToMemTypeCompositePtr dl tp = copilotTypeToMemType dl tp

-- | Convert a Copilot 'CT.Type' to an LLVM 'L.Type'. 'CT.Bool's are
-- assumed to be one bit in size. See @Note [How LLVM represents bool]@.
copilotTypeToLLVMType ::
  CT.Type a ->
  L.Type
copilotTypeToLLVMType = loop
  where
    loop :: forall t. CT.Type t -> L.Type
    loop CT.Bool   = L.PrimType (L.Integer 1)
    loop CT.Int8   = L.PrimType (L.Integer 8)
    loop CT.Int16  = L.PrimType (L.Integer 16)
    loop CT.Int32  = L.PrimType (L.Integer 32)
    loop CT.Int64  = L.PrimType (L.Integer 64)
    loop CT.Word8  = L.PrimType (L.Integer 8)
    loop CT.Word16 = L.PrimType (L.Integer 16)
    loop CT.Word32 = L.PrimType (L.Integer 32)
    loop CT.Word64 = L.PrimType (L.Integer 64)
    loop CT.Float  = L.PrimType (L.FloatType L.Float)
    loop CT.Double = L.PrimType (L.FloatType L.Double)
    loop t0@(CT.Array tp) =
      let len = fromIntegral (typeLength t0) in
      L.Array len (copilotTypeToLLVMTypeBool8 tp)
    loop (CT.Struct v) =
      -- NB: Don't use L.Struct here. That represents a literal, unnamed
      -- struct, but all of the structs used in a copilot-c99 program are
      -- named structs. As such, we must identify the struct by its alias.
      L.Alias (copilotStructIdent v)

-- | Like 'copilotTypeToLLVMType', except that 'CT.Bool's are assumed to be
-- eight bits, not one bit. See @Note [How LLVM represents bool]@.
copilotTypeToLLVMTypeBool8 ::
  CT.Type a ->
  L.Type
copilotTypeToLLVMTypeBool8 CT.Bool = L.PrimType (L.Integer 8)
copilotTypeToLLVMTypeBool8 tp = copilotTypeToLLVMType tp

-- | Like 'copilotTypeToLLVMType', except that composite types (i.e.,
-- 'CT.Array's and 'CT.Struct's) are given special treatment involving
-- pointers. See @Note [Arrays and structs]@.
copilotTypeToLLVMTypeCompositePtr ::
  CT.Type a ->
  L.Type
copilotTypeToLLVMTypeCompositePtr (CT.Array tp) =
  L.PtrTo (copilotTypeToLLVMTypeBool8 tp)
copilotTypeToLLVMTypeCompositePtr (CT.Struct struct) =
  L.PtrTo (L.Alias (copilotStructIdent struct))
copilotTypeToLLVMTypeCompositePtr tp = copilotTypeToLLVMType tp

-- | Given a struct @s@, construct the name @struct.s@ as an LLVM identifier.
copilotStructIdent :: Struct a => a -> L.Ident
copilotStructIdent struct = L.Ident $ "struct." ++ typeName struct

{-
Note [How LLVM represents bool]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How are C values of type `bool` represented in LLVM? It depends. If it's being
stored directly a `bool`, it's represented with `i1` (i.e., a single bit). If
a `bool` is a member of some composite type, such as a pointer, array, or
struct, however, it's representing with `i8` (i.e., eight bits). This means
that we have to be careful when converting Bool-typed Copilot values, as they
can become `i1` or `i8` depending on the context.

copilot-verifier handles this by having both `copilotTypeToLLVMType` and
`copilotTypeToLLVMTypeBool8` functions. The former function treats `bool`s as
`i1`, whereas the latter treats `bool`s as `i8`. The former is used when
converting "top-level" types (e.g., the argument types in a trigger override),
whereas the latter is used when converting types that are part of a larger
composite type (e.g., the element type in an array).

The story for the `copilotTypeToMemType` and `copilotTypeToMemTypeBool8`
functions is similar.

Note [Arrays and structs]
~~~~~~~~~~~~~~~~~~~~~~~~~
When Clang compiles a function with an array argument, such as this trigger
function:

  void func(int32_t func_arg0[2]) { ... }

It will produce the following LLVM code:

  declare void @func(i32*) { ... }

Note that the argument is an i32*, not a [2 x i32]. As a result, we can't
translate Copilot array types directly to LLVM array types when they're used as
arguments to a function. This impedance mismatch is handled in two places:

1. The `copilotTypeToMemTypeCompositePtr`/`copilotTypeToLLVMTypeCompositePtr`
   functions special-case Copilot arrays such that they are translated to
   pointers. These functions are used when declaring the argument types of an
   override for a trigger function (see `triggerOverride`).
2. The `computeEqualVals` function has a special case for pointer
   arguments—see the case that matches on `PtrRepr`. When a `PtrRepr` is
   encountered, the underlying array values that it points to are read from
   memory. Because `PtrRepr` doesn't record the type of the thing being pointed
   to, `computeEqualVals` uses the corresponding Copilot type as a guide to
   determine how much memory to read and at what type the memory should be
   used. After this, `computeEqualVals` reads from the read array
   element-by-element—see the `VectorRepr` cases.

   Note that unlike `computeEqualVals`, `copilotExprToRegValue` does not need
   a `PtrRepr` case. This is because `copilotExprToRegValue` is ultimately used
   in service of calling writing elements of streams to memory, and streams do
   not store pointer values (at least, not in today's Copilot).

There is a very similar story for structs. Copilot passes structs by reference
in trigger functions (e.g., `void trigger(struct s *ss)`), so we must also load
from a `PtrRepr` in `computeEqualVals` to handle structs.

See Note [Global variables for trigger functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As part of verifying that the behavior of a Copilot specification's trigger
functions behave the same way as the trigger functions in the corresponding C
program, we check that each trigger function in the C program is invoked the
appropriate number of times. That is, if the guard condition for a trigger is
true, the C trigger function should be invoked exactly once, and if the guard
condition is false, then the trigger function should not be invoked at all.

To check this, we create a Nat-valued global variable for each trigger function
and initialize it to zero. Whenever we simulate a trigger function, we increment
the value of the corresponding global variable. At the end of simulation, we
check that the value in each global variable is equal to
`if guard_cond then 1 else 0`.
-}

-- | Like @crucible-llvm@'s @doLoad@, but with the following differences:
--
-- * This function returns the 'BoolAnn' and 'Pred' asserting the validity of
--   the load, which the verifier needs for logging purposes.
--
-- * This always generates a durable proof goal so that Crucible will always
--   record it, even if it is trivial.
doLoadWithAnn ::
  ( IsSymBackend sym bak, HasPtrWidth wptr, HasLLVMAnn sym
  , ?memOpts :: MemOptions ) =>
  bak ->
  MemImpl sym ->
  LLVMPtr sym wptr {- ^ pointer to load from      -} ->
  StorageType      {- ^ type of value to load     -} ->
  TypeRepr tp      {- ^ crucible type of the result -} ->
  Alignment        {- ^ assumed pointer alignment -} ->
  IO (BoolAnn sym, Pred sym, RegValue sym tp)
doLoadWithAnn bak mem ptr valType tpr alignment = do
    partLLVMVal <- loadRaw sym mem ptr valType alignment
    (bann, p, llvmVal) <- assertSafeWithAnn bak partLLVMVal
    regVal <- unpackMemValue sym tpr llvmVal
    pure (bann, p, regVal)
  where
    sym = backendGetSym bak

-- | Like @crucible-llvm@'s @assertSafe@, but with the following differences:
--
-- * This function returns the 'BoolAnn' and 'Pred' corresponding to the
--   assertion, which the verifier needs for logging purposes.
--
-- * This generates a durable assertion so that Crucible will always record it,
--   even if it is trivial.
assertSafeWithAnn ::
  IsSymBackend sym bak =>
  bak ->
  PartLLVMVal sym ->
  IO (BoolAnn sym, Pred sym, LLVMVal sym)
assertSafeWithAnn bak partVal = do
  loc <- getCurrentProgramLoc sym
  let err = SimError loc rsn
  case partVal of
    NoErr p v -> do
      (ann, p') <- annotateTerm sym p
      addDurableAssertion bak (LabeledPred p' err)
      return (BoolAnn ann, p', v)
    Err p -> do
      (_ann, p') <- annotateTerm sym p
      addDurableProofObligation bak (LabeledPred p' err)
      abortExecBecause (AssertionFailure err)
  where
    rsn = AssertFailureSimError "Error during memory load" ""
    sym = backendGetSym bak

-- | Given a simulator state, extract any collected proof obligations,
--   attempt to prove them, and present the results to the user.
--
--   Afterward, the simulator state will be cleared of any proof obligations,
--   regardless of if they could all be proved.
proveObls ::
  IsSymInterface sym =>
  sym ~ ExprBuilder t st fs =>
  Log.Logs msgs =>
  Log.SupportsCruxLogMessage msgs =>
  Log.SupportsCopilotLogMessage msgs =>
  CruxOptions ->
  [SolverAdapter st] ->
  CopilotLogRefs sym ->
  Log.VerificationStep ->
  SimCtxt Crux sym LLVM ->
  IO Integer
proveObls cruxOpts adapters clRefs step simctx =
  withBackend simctx $ \bak ->
  do let sym = backendGetSym bak
     obls <- getProofObligations bak
     clearProofObligations bak

--     mapM_ (print . ppSimError) (summarizeObls sym obls)

     vaMap <- readIORef $ verifierAssertionMapRef clRefs
     let laMapRef = llvmAnnMapRef clRefs
     laMap <- readIORef laMapRef
     results <- proveGoalsOffline adapters cruxOpts simctx (explainFailure sym laMapRef) obls
     presentResults sym vaMap laMap step results

{-
summarizeObls :: sym -> ProofObligations sym -> [SimError]
summarizeObls _ Nothing = []
summarizeObls _ (Just obls) = map (view labeledPredMsg . proofGoal) (goalsToList obls)
-}

-- | Compute the number of goals that were proven during verification, logging
-- the goals as they are proven. If all goals are proven successfully, return
-- the number of goals. Otherwise, log the failing proof goals and abort.
presentResults ::
  Log.Logs msgs =>
  Log.SupportsCopilotLogMessage msgs =>
  IsSymInterface sym =>
  sym ->
  Map.Map (BoolAnn sym) (Log.VerifierAssertion sym) ->
  LLVMAnnMap sym ->
  Log.VerificationStep ->
  (ProcessedGoals, Maybe (Goals (Assumptions sym) (Assertion sym, [ProgramLoc], ProofResult sym))) ->
  IO Integer
presentResults sym vaMap laMap step (num, goals)
  | numTotalGoals == 0
  = do Log.sayCopilot Log.AllGoalsProved
       pure 0

    -- All goals were proven
  | numProvedGoals == numTotalGoals
  = do traverse_ (logVerifierAssertions sym vaMap laMap step num) goals
       printGoals
       pure numProvedGoals

    -- There were some unproved goals, so fail with exit code 1
  | otherwise
  = do printGoals
       exitFailure
  where
    numTotalGoals  = totalProcessedGoals num
    numProvedGoals = provedGoals num

    printGoals =
      do Log.sayCopilot $ Log.OnlySomeGoalsProved numProvedGoals numTotalGoals
         goals' <- provedGoalsTree sym goals
         case goals' of
           Just g -> Log.logGoal g
           Nothing -> return ()

-- | Upon a successful verification, log the various assertions that the
-- verifier makes. These assertions will be visible in the output if the
-- 'verbosity' is set to 'Noisy'.
logVerifierAssertions ::
  forall sym msgs.
  IsSymInterface sym =>
  Log.Logs msgs =>
  Log.SupportsCopilotLogMessage msgs =>
  sym ->
  Map.Map (BoolAnn sym) (Log.VerifierAssertion sym) ->
  LLVMAnnMap sym ->
  Log.VerificationStep ->
  ProcessedGoals ->
  Goals (Assumptions sym) (Assertion sym, [ProgramLoc], ProofResult sym) ->
  IO ()
logVerifierAssertions sym vaMap laMap step num goals = void $ go 0 goals
  where
    numTotalGoals = totalProcessedGoals num

    go :: Integer ->
          Goals (Assumptions sym) (Assertion sym, [ProgramLoc], ProofResult sym) ->
          IO Integer
    go goalIdx gs =
      case gs of
        Assuming _ gs' ->
          go goalIdx gs'

        Prove (gl, locs, _) -> do
          let p = gl^.labeledPred
              nearestLoc = nearestProgramLoc locs

          -- First, obtain the verifier assertion.
          va <- case getAnnotation sym p of
            -- If the assertion has a BoolAnn, look it up in the assertion maps
            -- that we have accumulated during verification.
            Just ann
              |  Just va <- Map.lookup (BoolAnn ann) vaMap
              -> pure va
              |  Just (stk, bb) <- Map.lookup (BoolAnn ann) laMap
              -> pure $ Log.LLVMBadBehaviorCheckAssertion
                      $ Log.LLVMBadBehaviorCheck sym nearestLoc stk bb p
              |  otherwise
              -> fail $ unlines
                   [ "Cannot find BoolAnn for assertion"
                   , show $ gl^.labeledPredMsg
                   , show $ printSymExpr p
                   ]
            -- If the assertion does not have a BoolAnn, fall back to using
            -- heuristics to guess what kind of assertion it is.
            Nothing -> pure $ verifierAssertionHeuristics sym nearestLoc p

          -- Log the assertion.
          case va of
            Log.StreamValueEqualityAssertion (Log.SomeSome a) ->
              Log.sayCopilot $
              Log.StreamValueEqualityProofGoal step goalIdx numTotalGoals a
            Log.TriggersInvokedCorrespondinglyAssertion a ->
              Log.sayCopilot $
              Log.TriggersInvokedCorrespondinglyProofGoal step goalIdx numTotalGoals a
            Log.TriggerArgumentEqualityAssertion (Log.SomeSome a) ->
              Log.sayCopilot $
              Log.TriggerArgumentEqualityProofGoal step goalIdx numTotalGoals a
            Log.RingBufferLoadAssertion (Log.SomeSome a) ->
              Log.sayCopilot $
              Log.RingBufferLoadProofGoal step goalIdx numTotalGoals a
            Log.RingBufferIndexLoadAssertion a ->
              Log.sayCopilot $
              Log.RingBufferIndexLoadProofGoal step goalIdx numTotalGoals a
            Log.PointerArgumentLoadAssertion (Log.SomeSome a) ->
              Log.sayCopilot $
              Log.PointerArgumentLoadProofGoal step goalIdx numTotalGoals a
            Log.AccessorFunctionLoadAssertion a ->
              Log.sayCopilot $
              Log.AccessorFunctionLoadProofGoal step goalIdx numTotalGoals a
            Log.GuardFunctionLoadAssertion a ->
              Log.sayCopilot $
              Log.GuardFunctionLoadProofGoal step goalIdx numTotalGoals a
            Log.UnknownFunctionLoadAssertion a ->
              Log.sayCopilot $
              Log.UnknownFunctionLoadProofGoal step goalIdx numTotalGoals a
            Log.LLVMBadBehaviorCheckAssertion a ->
              Log.sayCopilot $
              Log.LLVMBadBehaviorCheckProofGoal step goalIdx numTotalGoals a

          -- Finally, return the current goal index.
          pure goalIdx

        ProveConj gs1 gs2 -> do
          goalIdx' <- go goalIdx gs1
          go (goalIdx' + 1) gs2

-- | If a verifier assertion does not have a corresponding 'BoolAnn', then we
-- must use heuristics to guess what kind of assertion it is. These heuristics
-- are not perfect, and we fall back to 'Log.UnknownFunctionLoad' in the event
-- that we cannot figure out a more obvious cause for the assertion.
verifierAssertionHeuristics ::
  IsSymInterface sym =>
  sym ->
  ProgramLoc ->
  Pred sym ->
  Log.VerifierAssertion sym
verifierAssertionHeuristics sym loc p
  | "_get" `Text.isSuffixOf` functionName funName
  = Log.AccessorFunctionLoadAssertion $
    Log.AccessorFunctionLoad sym loc funName p

  | "_guard" `Text.isSuffixOf` functionName funName
  = Log.GuardFunctionLoadAssertion $
    Log.GuardFunctionLoad sym loc funName p

  | otherwise
  = Log.UnknownFunctionLoadAssertion $
    Log.UnknownFunctionLoad sym loc funName p
  where
    funName = plFunction loc

-- | Pick the most recent 'ProgramLoc' in a trace of locations. If there are
-- no locations available, return a dummy location.
nearestProgramLoc :: [ProgramLoc] -> ProgramLoc
nearestProgramLoc locs =
  case locs of
    loc:_ -> loc
    _     -> mkProgramLoc "<>" InternalPos

-- | A collection of 'IORef's used to accumulate log messages that the verifier
-- may display at the end of verification.
data CopilotLogRefs sym = CopilotLogRefs
  { verifierAssertionMapRef :: !(IORef (Map.Map (BoolAnn sym) (Log.VerifierAssertion sym)))
    -- ^ A map of 'BoolAnn's (i.e., unique numbers) to 'Log.VerifierAssertions'.
  , llvmAnnMapRef :: !(IORef (LLVMAnnMap sym))
    -- ^ A map of 'BoolAnn's (i.e., unique numbers) to assertions about checks
    -- for bad behavior in LLVM.

    -- This is kept in a separate 'IORef' for technical reasons, as
    -- @crucible-llvm@'s 'explainFailure' function expects this 'IORef' as an
    -- argument. We could put everything into 'verifierAssertionMapRef', but
    -- that would require some tiresome 'IORef' massaging to make work.
  }

-- | Create a new 'CopilotLogRefs' value.
newCopilotLogRefs :: IsSymInterface sym => IO (CopilotLogRefs sym)
newCopilotLogRefs = do
  vaMapRef <- newIORef mempty
  laMapRef <- newIORef mempty
  pure $ CopilotLogRefs
    { verifierAssertionMapRef = vaMapRef
    , llvmAnnMapRef = laMapRef
    }

data CopilotLogging
  = LoggingCrux Log.CruxLogMessage
  | LoggingCruxLLVM Log.CruxLLVMLogMessage
  | LoggingCopilot Log.CopilotLogMessage
  deriving stock Generic
  deriving anyclass ToJSON

copilotLoggingToSayWhat :: CopilotLogging -> Log.SayWhat
copilotLoggingToSayWhat (LoggingCrux msg) = Log.cruxLogMessageToSayWhat msg
copilotLoggingToSayWhat (LoggingCruxLLVM msg) = Log.cruxLLVMLogMessageToSayWhat msg
copilotLoggingToSayWhat (LoggingCopilot msg) = Log.copilotLogMessageToSayWhat msg

withCopilotLogging ::
  ( ( Log.SupportsCruxLogMessage CopilotLogging
    , Log.SupportsCruxLLVMLogMessage CopilotLogging
    , Log.SupportsCopilotLogMessage CopilotLogging
    ) => computation ) ->
  computation
withCopilotLogging computation = do
  let ?injectCruxLogMessage = LoggingCrux
      ?injectCruxLLVMLogMessage = LoggingCruxLLVM
      ?injectCopilotLogMessage = LoggingCopilot
    in computation

sayTranslationWarning ::
  Log.Logs msgs =>
  Log.SupportsCruxLLVMLogMessage msgs =>
  LLVMTranslationWarning -> IO ()
sayTranslationWarning = Log.sayCruxLLVM . f
  where
    f (LLVMTranslationWarning s p msg) =
        Log.TranslationWarning (Text.pack (show (ppSymbol s))) (Text.pack (show p)) msg
