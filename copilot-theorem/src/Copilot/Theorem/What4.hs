{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      : Copilot.Theorem.What4
-- Description : Prove spec properties using What4.
-- Copyright   : (c) Ben Selfridge, 2020
-- Maintainer  : benselfridge@galois.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Spec properties are translated into the language of SMT solvers using
-- @What4@. A backend solver is then used to prove the property is true. The
-- technique is sound, but incomplete. If a property is proved true by this
-- technique, then it can be guaranteed to be true. However, if a property is
-- not proved true, that does not mean it isn't true; the proof may fail because
-- the given property is not inductive.
--
-- We perform @k@-induction on all the properties in a given specification where
-- @k@ is chosen to be the maximum amount of delay on any of the involved
-- streams. This is a heuristic choice, but often effective.
module Copilot.Theorem.What4
  ( -- * Proving properties about Copilot specifications
    prove
  , Solver(..)
  , SatResult(..)
  , proveWithCounterExample
  , SatResultCex(..)
  , CounterExample(..)
    -- * Bisimulation proofs about @copilot-c99@ code
  , computeBisimulationProofBundle
  , BisimulationProofBundle(..)
  , BisimulationProofState(..)
    -- * What4 representations of Copilot expressions
  , XExpr(..)
  , CopilotValue(..)
  , StreamOffset(..)
  ) where

import qualified Copilot.Core.Expr       as CE
import qualified Copilot.Core.Spec       as CS
import qualified Copilot.Core.Type       as CT
import qualified Copilot.Core.Type.Array as CTA

import qualified What4.Config                   as WC
import qualified What4.Expr.Builder             as WB
import qualified What4.Expr.GroundEval          as WG
import qualified What4.Interface                as WI
import qualified What4.InterpretedFloatingPoint as WFP
import qualified What4.Solver                   as WS
import qualified What4.Solver.DReal             as WS

import Control.Monad (forM)
import Control.Monad.State
import qualified Data.BitVector.Sized as BV
import Data.Foldable (foldrM)
import Data.List (genericLength)
import qualified Data.Map as Map
import Data.Parameterized.NatRepr
import Data.Parameterized.Nonce
import Data.Parameterized.Some
import qualified Data.Parameterized.Vector as V
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import LibBF (BigFloat, bfToDouble, pattern NearEven)
import qualified Panic as Panic

import Copilot.Theorem.What4.Translate

-- 'prove' function
--
-- To prove properties of a spec, we translate them into What4 using the TransM
-- monad (transformer on top of IO), then negate each property and ask a backend
-- solver to produce a model for the negation.

-- | No builder state needed.
data BuilderState a = EmptyState

-- | The solvers supported by the what4 backend.
data Solver = CVC4 | DReal | Yices | Z3

-- | The 'prove' function returns results of this form for each property in a
-- spec.
data SatResult = Valid | Invalid | Unknown
  deriving Show

-- | The 'proveWithCounterExample' function returns results of this form for
-- each property in a spec. This is largely the same as 'SatResult', except that
-- 'InvalidCex' also records a 'CounterExample'.
data SatResultCex = ValidCex | InvalidCex CounterExample | UnknownCex

-- | Concrete values that cause a property in a Copilot specification to be
-- invalid. As a simple example, consider the following spec:
--
-- @
-- spec :: Spec
-- spec = do
--   let s :: Stream Bool
--       s = [False] ++ constant True
--   void $ prop "should be invalid" (forAll s)
-- @
--
-- This defines a stream @s@ where the first value is @False@, but all
-- subsequent values are @True@'. This is used in a property that asserts that
-- the values in @s@ will be @True@ at all possible time steps. This is clearly
-- not true, given that @s@'s first value is @False@. As such, we would expect
-- that proving this property would yield an 'InvalidCex' result, where one of
-- the base cases would state that the @s@ stream contains a @False@ value.
--
-- We can use the 'proveWithCounterExample' function to query an SMT solver to
-- compute a counterexample:
--
-- @
-- CounterExample
--   { 'baseCases' =
--       [False]
--   , 'inductionStep' =
--       True
--   , 'concreteExternVars' =
--       fromList []
--   , 'concreteStreamValues' =
--       fromList
--         [ ( (0, 'AbsoluteOffset' 0), False )
--         , ( (0, 'RelativeOffset' 0), False )
--         , ( (0, 'RelativeOffset' 1), True )
--         ]
--   }
-- @
--
-- Let's go over what this counterexample is saying:
--
-- * The 'inductionStep' of the proof is 'True', so that part of the proof was
--   successful. On the other hand, the 'baseCases' contain a 'False', so the
--   proof was falsified when proving the base cases. (In this case, the list
--   has only one element, so there is only a single base case.)
--
-- * 'concreteStreamValues' reports the SMT solver's concrete values for each
--   stream during relevant parts of the proof as a 'Map.Map'.
--
--   The keys of the map are pairs. The first element of the pair is the stream
--   'CE.Id', and in this example, the only 'CE.Id' is @0@, corresponding to the
--   stream @s@. The second element is the time offset. An 'AbsoluteOffset'
--   indicates an offset starting from the initial time step, and a
--   'RelativeOffset' indicates an offset from an arbitrary point in time.
--   'AbsoluteOffset's are used in the base cases of the proof, and
--   'RelativeOffset's are used in the induction step of the proof.
--
--   The part of the map that is most interesting to us is the
--   @( (0, 'AbsoluteOffset' 0), False )@ entry, which represents a base case
--   where there is a value of @False@ in the stream @s@ during the initial time
--   step. Sure enough, this is enough to falsify the property @forAll s@.
--
-- * There are no extern streams in this example, so 'concreteExternVars' is
--   empty.
--
-- We can also see an example of where a proof succeeds in the base cases, but
-- fails during the induction step:
--
-- @
-- spec :: Spec
-- spec = do
--   let t :: Stream Bool
--       t = [True] ++ constant False
--   void $ prop "should also be invalid" (forAll t)
-- @
--
-- With the @t@ stream above, the base cases will succeed
-- ('proveWithCounterExample' uses @k@-induction with a value of @k == 1@ in
-- this example, so there will only be a single base case). On the other hand,
-- the induction step will fail, as later values in the stream will be @False@.
-- If we try to 'proveWithCounterExample' this property, then it will fail with:
--
-- @
-- CounterExample
--   { 'baseCases' =
--       [True]
--   , 'inductionStep' =
--       False
--   , 'concreteExternVars' =
--       fromList []
--   , 'concreteStreamValues' =
--       fromList
--         [ ( (0, 'AbsoluteOffset' 0), True )
--         , ( (0, 'RelativeOffset' 0), True )
--         , ( (0, 'RelativeOffset' 1), False )
--         ]
--   }
-- @
--
-- This time, the 'inductionStep' is 'False'. If we look at the
-- 'concreteStreamValues', we see the values at @'RelativeOffset' 0@ and
-- @'RelativeOffset' 1@ (which are relevant to the induction step) are @True@
-- and @False@, respectively. Since this is a proof by @k@-induction where
-- @k == 1@, the fact that the value at @'RelativeOffset 1@ is @False@ indicates
-- that the induction step was falsified.
--
-- Note that this proof does not say /when/ exactly the time steps at
-- @'RelativeOffset' 0@ or @'RelativeOffset' 1@ occur, only that that will occur
-- relative to some arbitrary point in time. In this example, they occur
-- relative to the initial time step, so @'RelativeOffset' 1@ would occur at the
-- second time step overall. In general, however, these time steps may occur far
-- in the future, so it is possible that one would need to step through the
-- execution of the streams for quite some time before finding the
-- counterexample.
--
-- Be aware that counterexamples involving struct values are not currently
-- supported.
data CounterExample = CounterExample
  { -- | A list of base cases in the proof, where each entry in the list
    -- corresponds to a particular time step. For instance, the first element
    -- in the list corresponds to the initial time step, the second element in
    -- the list corresponds to the second time step, and so on. A 'False' entry
    -- anywhere in this list will cause the overall proof to be 'InvalidCex'.
    --
    -- Because the proof uses @k@-induction, the number of base cases (i.e., the
    -- number of entries in this list) is equal to the value of @k@, which is
    -- chosen using heuristics.
    baseCases :: [Bool]
    -- | Whether the induction step of the proof was valid or not. That is,
    -- given an arbitrary time step @n@, if the property is assumed to hold at
    -- time steps @n@, @n+1@, ..., @n+k@, then this will be @True@ is the
    -- property can the be proven to hold at time step @n+k+1@ (and 'False'
    -- otherwise). If this is 'False', then the overall proof will be
    -- 'InvalidCex'.
  , inductionStep :: Bool
    -- | The concrete values in the Copilot specification's extern streams that
    -- lead to the property being invalid.
    --
    -- Each key in the 'Map.Map' is the 'CE.Name' of an extern stream paired
    -- with a 'StreamOffset' representing the time step. The key's corresponding
    -- value is the concrete value of the extern stream at that time step.
  , concreteExternValues :: Map.Map (CE.Name, StreamOffset) (Some CopilotValue)
    -- | The concrete values in the Copilot specification's streams (excluding
    -- extern streams) that lead to the property being invalid.
    --
    -- Each key in the 'Map.Map' is the 'CE.Id' of a stream paired with a
    -- 'StreamOffset' representing the time step. The key's corresponding value
    -- is the concrete value of the extern stream at that time step.
  , concreteStreamValues :: Map.Map (CE.Id, StreamOffset) (Some CopilotValue)
  }

-- | Attempt to prove all of the properties in a spec via an SMT solver (which
-- must be installed locally on the host). Return an association list mapping
-- the names of each property to the result returned by the solver.
prove :: Solver
      -- ^ Solver to use
      -> CS.Spec
      -- ^ Spec
      -> IO [(CE.Name, SatResult)]
prove solver spec = proveInternal solver spec $ \_ _ _ satRes ->
  case satRes of
    WS.Sat _   -> pure Invalid
    WS.Unsat _ -> pure Valid
    WS.Unknown -> pure Unknown

-- | Attempt to prove all of the properties in a spec via an SMT solver (which
-- must be installed locally on the host). Return an association list mapping
-- the names of each property to the result returned by the solver.
--
-- Unlike 'prove', 'proveWithCounterExample' returns a 'SatResultCex'. This
-- means that if a result is invalid, then it will include a 'CounterExample'
-- which describes the circumstances under which the property was falsified. See
-- the Haddocks for 'CounterExample' for more details.
--
-- Note that this function does not currently support creating counterexamples
-- involving struct values, so attempting to call 'proveWithCounterExample' on a
-- specification that uses structs will raise an error.
proveWithCounterExample :: Solver
                        -- ^ Solver to use
                        -> CS.Spec
                        -- ^ Spec
                        -> IO [(CE.Name, SatResultCex)]
proveWithCounterExample solver spec =
  proveInternal solver spec $ \baseCases indStep st satRes ->
    case satRes of
      WS.Sat ge -> do
        gBaseCases <- traverse (WG.groundEval ge) baseCases
        gIndStep <- WG.groundEval ge indStep
        gExternValues <- traverse (valFromExpr ge) (externVars st)
        gStreamValues <- traverse (valFromExpr ge) (streamValues st)
        let cex = CounterExample
              { baseCases            = gBaseCases
              , inductionStep        = gIndStep
              , concreteExternValues = gExternValues
              , concreteStreamValues = gStreamValues
              }
        pure (InvalidCex cex)
      WS.Unsat _ -> pure ValidCex
      WS.Unknown -> pure UnknownCex

-- | Attempt to prove all of the properties in a spec via an SMT solver (which
-- must be installed locally on the host). For each 'WS.SatResult' returned by
-- the solver, pass it to a continuation along with the relevant parts of the
-- proof-related state.
--
-- This is an internal-only function that is used to power 'prove' and
-- 'proveWithCounterExample'.
proveInternal :: Solver
              -- ^ Solver to use
              -> CS.Spec
              -- ^ Spec
              -> (forall sym t st fm
                   . ( sym ~ WB.ExprBuilder t st (WB.Flags fm)
                     , WI.KnownRepr WB.FloatModeRepr fm )
                  => [WI.Pred sym]
                     -- The proof's base cases
                  -> WI.Pred sym
                     -- The proof's induction step
                  -> TransState sym
                     -- The proof state
                  -> WS.SatResult (WG.GroundEvalFn t) ()
                     -- The overall result of the proof
                  -> IO a)
              -- ^ Continuation to call on each solver result
              -> IO [(CE.Name, a)]
proveInternal solver spec k = do
  -- Setup symbolic backend
  Some ng <- newIONonceGenerator
  sym <- WB.newExprBuilder WB.FloatIEEERepr EmptyState ng

  -- Solver-specific options
  case solver of
    CVC4 -> WC.extendConfig WS.cvc4Options (WI.getConfiguration sym)
    DReal -> WC.extendConfig WS.drealOptions (WI.getConfiguration sym)
    Yices -> WC.extendConfig WS.yicesOptions (WI.getConfiguration sym)
    Z3 -> WC.extendConfig WS.z3Options (WI.getConfiguration sym)

  -- Compute the maximum amount of delay for any stream in this spec
  let bufLen (CS.Stream _ buf _ _) = genericLength buf
      maxBufLen = maximum (0 : (bufLen <$> CS.specStreams spec))

  -- This process performs k-induction where we use @k = maxBufLen@.
  -- The choice for @k@ is heuristic, but often effective.
  let proveProperties = forM (CS.specProperties spec) $ \pr -> do
        -- State the base cases for k induction.
        base_cases <- forM [0 .. maxBufLen - 1] $ \i -> do
          xe <- translateExpr sym mempty (CS.propertyExpr pr) (AbsoluteOffset i)
          case xe of
            XBool p -> return p
            _ -> expectedBool "Property" xe

        -- Translate the induction hypothesis for all values up to maxBufLen in
        -- the past
        ind_hyps <- forM [0 .. maxBufLen-1] $ \i -> do
          xe <- translateExpr sym mempty (CS.propertyExpr pr) (RelativeOffset i)
          case xe of
            XBool hyp -> return hyp
            _ -> expectedBool "Property" xe

        -- Translate the predicate for the "current" value
        ind_goal <- do
          xe <- translateExpr sym
                              mempty
                              (CS.propertyExpr pr)
                              (RelativeOffset maxBufLen)
          case xe of
            XBool p -> return p
            _ -> expectedBool "Property" xe

        -- Compute the predicate (ind_hyps ==> p)
        ind_case <- liftIO $ foldrM (WI.impliesPred sym) ind_goal ind_hyps

        -- Compute the conjunction of the base and inductive cases
        p <- liftIO $ foldrM (WI.andPred sym) ind_case base_cases

        -- Negate the goals for for SAT search
        not_p <- liftIO $ WI.notPred sym p
        let clauses = [not_p]

        st <- get
        let k' = k base_cases ind_case st
        satRes <-
          case solver of
            CVC4 -> liftIO $ WS.runCVC4InOverride sym WS.defaultLogData clauses $ \case
              WS.Sat (ge, _) -> k' (WS.Sat ge)
              WS.Unsat x -> k' (WS.Unsat x)
              WS.Unknown -> k' WS.Unknown
            DReal -> liftIO $ WS.runDRealInOverride sym WS.defaultLogData clauses $ \case
              WS.Sat (c, m) -> do
                ge <- WS.getAvgBindings c m
                k' (WS.Sat ge)
              WS.Unsat x -> k' (WS.Unsat x)
              WS.Unknown -> k' WS.Unknown
            Yices -> liftIO $ WS.runYicesInOverride sym WS.defaultLogData clauses $ \case
              WS.Sat ge -> k' (WS.Sat ge)
              WS.Unsat x -> k' (WS.Unsat x)
              WS.Unknown -> k' WS.Unknown
            Z3 -> liftIO $ WS.runZ3InOverride sym WS.defaultLogData clauses $ \case
              WS.Sat (ge, _) -> k' (WS.Sat ge)
              WS.Unsat x -> k' (WS.Unsat x)
              WS.Unknown -> k' WS.Unknown

        pure (CS.propertyName pr, satRes)

  -- Execute the action and return the results for each property
  runTransM spec proveProperties

-- Bisimulation proofs

-- | Given a Copilot specification, compute all of the symbolic states necessary
-- to carry out a bisimulation proof that establishes a correspondence between
-- the states of the Copilot stream program and the C code that @copilot-c99@
-- would generate for that Copilot program.
computeBisimulationProofBundle ::
     WFP.IsInterpretedFloatSymExprBuilder sym
  => sym
  -> [String]
  -- ^ Names of properties to assume during verification
  -> CS.Spec
  -- ^ The input Copilot specification
  -> IO (BisimulationProofBundle sym)
computeBisimulationProofBundle sym properties spec = do
  iss <- computeInitialStreamState sym spec
  runTransM spec $ do
    prestate  <- computePrestate sym spec
    poststate <- computePoststate sym spec
    triggers  <- computeTriggerState sym spec
    assms     <- computeAssumptions sym properties spec
    externs   <- computeExternalInputs sym
    sideCnds  <- gets sidePreds
    return
      BisimulationProofBundle
        { initialStreamState = iss
        , preStreamState     = prestate
        , postStreamState    = poststate
        , externalInputs     = externs
        , triggerState       = triggers
        , assumptions        = assms
        , sideConds          = sideCnds
        }

-- | A collection of all of the symbolic states necessary to carry out a
-- bisimulation proof.
data BisimulationProofBundle sym =
  BisimulationProofBundle
    { initialStreamState :: BisimulationProofState sym
      -- ^ The state of the global variables at program startup
    , preStreamState :: BisimulationProofState sym
      -- ^ The stream state prior to invoking the step function
    , postStreamState :: BisimulationProofState sym
      -- ^ The stream state after invoking the step function
    , externalInputs :: [(CE.Name, Some CT.Type, XExpr sym)]
      -- ^ A list of external streams, where each tuple contains:
      --
      -- 1. The name of the stream
      --
      -- 2. The type of the stream
      --
      -- 3. The value of the stream represented as a fresh constant
    , triggerState :: [(CE.Name, WI.Pred sym, [(Some CT.Type, XExpr sym)])]
      -- ^ A list of trigger functions, where each tuple contains:
      --
      -- 1. The name of the function
      --
      -- 2. A formula representing the guarded condition
      --
      -- 3. The arguments to the function, where each argument is represented as
      --    a type-value pair
    , assumptions :: [WI.Pred sym]
      -- ^ User-provided property assumptions
    , sideConds :: [WI.Pred sym]
      -- ^ Side conditions related to partial operations
    }

-- | The state of a bisimulation proof at a particular step.
newtype BisimulationProofState sym =
  BisimulationProofState
    { streamState :: [(CE.Id, Some CT.Type, [XExpr sym])]
      -- ^ A list of tuples containing:
      --
      -- 1. The name of a stream
      --
      -- 2. The type of the stream
      --
      -- 3. The list of values in the stream description
    }

-- | Compute the initial state of the global variables at the start of a Copilot
-- program.
computeInitialStreamState ::
     WFP.IsInterpretedFloatSymExprBuilder sym
  => sym
  -> CS.Spec
  -- ^ The input Copilot specification
  -> IO (BisimulationProofState sym)
computeInitialStreamState sym spec = do
  xs <- forM (CS.specStreams spec) $
         \CS.Stream { CS.streamId = nm, CS.streamExprType = tp
                    , CS.streamBuffer = buf } ->
         do vs <- mapM (translateConstExpr sym tp) buf
            return (nm, Some tp, vs)
  return (BisimulationProofState xs)

-- | Compute the stream state of a Copilot program prior to invoking the step
-- function.
computePrestate ::
     WFP.IsInterpretedFloatSymExprBuilder sym
  => sym
  -> CS.Spec
  -- ^ The input Copilot specification
  -> TransM sym (BisimulationProofState sym)
computePrestate sym spec = do
  xs <- forM (CS.specStreams spec) $
          \CS.Stream { CS.streamId = nm, CS.streamExprType = tp
                     , CS.streamBuffer = buf } ->
          do let buflen = genericLength buf
             let idxes = RelativeOffset <$> [0 .. buflen-1]
             vs <- mapM (getStreamValue sym nm) idxes
             return (nm, Some tp, vs)
  return (BisimulationProofState xs)

-- | Compute ehe stream state of a Copilot program after invoking the step
-- function.
computePoststate ::
     WFP.IsInterpretedFloatSymExprBuilder sym
  => sym
  -> CS.Spec
  -- ^ The input Copilot specification
  -> TransM sym (BisimulationProofState sym)
computePoststate sym spec = do
  xs <- forM (CS.specStreams spec) $
          \CS.Stream { CS.streamId = nm, CS.streamExprType = tp
                     , CS.streamBuffer = buf } ->
          do let buflen = genericLength buf
             let idxes = RelativeOffset <$> [1 .. buflen]
             vs <- mapM (getStreamValue sym nm) idxes
             return (nm, Some tp, vs)
  return (BisimulationProofState xs)

-- | Compute the trigger functions in a Copilot program.
computeTriggerState ::
     WFP.IsInterpretedFloatSymExprBuilder sym
  => sym
  -> CS.Spec
  -- ^ The input Copilot specification
  -> TransM sym [(CE.Name, WI.Pred sym, [(Some CT.Type, XExpr sym)])]
computeTriggerState sym spec = forM (CS.specTriggers spec) $
    \(CS.Trigger { CS.triggerName = nm, CS.triggerGuard = guard
                 , CS.triggerArgs = args }) ->
      do xguard <- translateExpr sym mempty guard (RelativeOffset 0)
         guard' <-
           case xguard of
             XBool guard' -> return guard'
             _ -> expectedBool "Trigger guard" xguard
         args' <- mapM computeArg args
         return (nm, guard', args')
  where
   computeArg (CE.UExpr tp ex) = do
     v <- translateExpr sym mempty ex (RelativeOffset 0)
     return (Some tp, v)

-- | Compute the values of the external streams in a Copilot program, where each
-- external stream is represented as a fresh constant.
computeExternalInputs ::
     WFP.IsInterpretedFloatSymExprBuilder sym
  => sym
  -> TransM sym [(CE.Name, Some CT.Type, XExpr sym)]
computeExternalInputs sym = do
  exts <- Map.toList <$> gets mentionedExternals
  forM exts $ \(nm, Some tp) -> do
    v <- getExternConstant sym tp nm (RelativeOffset 0)
    return (nm, Some tp, v)

-- | Compute the user-provided property assumptions in a Copilot program.
computeAssumptions ::
     forall sym.
     WFP.IsInterpretedFloatSymExprBuilder sym
  => sym
  -> [String]
  -- ^ Names of properties to assume during verification
  -> CS.Spec
  -- ^ The input Copilot specification
  -> TransM sym [WI.Pred sym]
computeAssumptions sym properties spec =
    concat <$> forM specPropertyExprs computeAssumption
  where
    bufLen (CS.Stream _ buf _ _) = genericLength buf
    maxBufLen = maximum (0 : (bufLen <$> CS.specStreams spec))

    -- Retrieve the boolean-values Copilot expressions corresponding to the
    -- user-provided property assumptions.
    specPropertyExprs :: [CE.Expr Bool]
    specPropertyExprs =
      [ CS.propertyExpr p
      | p <- CS.specProperties spec
      , elem (CS.propertyName p) properties
      ]

    -- Compute all of the what4 predicates corresponding to each user-provided
    -- property assumption.
    computeAssumption :: CE.Expr Bool -> TransM sym [WI.Pred sym]
    computeAssumption e = forM [0 .. maxBufLen] $ \i -> do
      xe <- translateExpr sym mempty e (RelativeOffset i)
      case xe of
        XBool b -> return b
        _ -> expectedBool "Property" xe

-- * Auxiliary functions

-- | A catch-all 'panic' to use when an 'XExpr' is is expected to uphold the
-- invariant that it is an 'XBool', but the invariant is violated.
expectedBool :: forall m sym a.
                (Panic.HasCallStack, MonadIO m, WI.IsExprBuilder sym)
             => String
             -- ^ What the 'XExpr' represents
             -> XExpr sym
             -> m a
expectedBool what xe =
  panic [what ++ " expected to have boolean result", show xe]

-- | A Copilot value paired with its 'CT.Type'.
data CopilotValue a where
  CopilotValue :: CT.Typed a => CT.Type a -> a -> CopilotValue a

-- | Convert a symbolic 'XExpr' into a concrete 'CopilotValue'.
--
-- Struct values are not currently supported, so attempting to convert an
-- 'XStruct' value will raise an error.
valFromExpr :: forall sym t st fm.
               ( sym ~ WB.ExprBuilder t st (WB.Flags fm)
               , WI.KnownRepr WB.FloatModeRepr fm
               )
            => WG.GroundEvalFn t
            -> XExpr sym
            -> IO (Some CopilotValue)
valFromExpr ge xe = case xe of
  XBool e -> Some . CopilotValue CT.Bool <$> WG.groundEval ge e
  XInt8 e -> Some . CopilotValue CT.Int8 . fromBV <$> WG.groundEval ge e
  XInt16 e -> Some . CopilotValue CT.Int16 . fromBV <$> WG.groundEval ge e
  XInt32 e -> Some . CopilotValue CT.Int32 . fromBV <$> WG.groundEval ge e
  XInt64 e -> Some . CopilotValue CT.Int64 . fromBV <$> WG.groundEval ge e
  XWord8 e -> Some . CopilotValue CT.Word8 . fromBV <$> WG.groundEval ge e
  XWord16 e -> Some . CopilotValue CT.Word16 . fromBV <$> WG.groundEval ge e
  XWord32 e -> Some . CopilotValue CT.Word32 . fromBV <$> WG.groundEval ge e
  XWord64 e -> Some . CopilotValue CT.Word64 . fromBV <$> WG.groundEval ge e
  XFloat e ->
    Some . CopilotValue CT.Float <$>
      iFloatGroundEval WFP.SingleFloatRepr e
                       (realToFrac . fst . bfToDouble NearEven)
                       fromRational
                       (castWord32ToFloat . fromInteger . BV.asUnsigned)
  XDouble e ->
    Some . CopilotValue CT.Double <$>
      iFloatGroundEval WFP.DoubleFloatRepr e
                       (fst . bfToDouble NearEven)
                       fromRational
                       (castWord64ToDouble . fromInteger . BV.asUnsigned)
  XEmptyArray tp ->
    pure $ Some $ CopilotValue (CT.Array @0 tp) (CTA.array [])
  XArray es -> do
    (someCVs :: V.Vector n (Some CopilotValue)) <- traverse (valFromExpr ge) es
    (Some (CopilotValue headTp _headVal), _) <- pure $ V.uncons someCVs
    cvs <-
      traverse
        (\(Some (CopilotValue tp val)) ->
          case tp `testEquality` headTp of
            Just Refl -> pure val
            Nothing -> panic [ "XArray with mismatched element types"
                             , show tp
                             , show headTp
                             ])
        someCVs
    pure $ Some $ CopilotValue (CT.Array @n headTp) (CTA.array (V.toList cvs))
  XStruct _ -> error "valFromExpr: Structs not currently handled"
  where
    fromBV :: forall a w . Num a => BV.BV w -> a
    fromBV = fromInteger . BV.asUnsigned

    -- Evaluate a (possibly symbolic) floating-point value to a concrete result.
    -- Depending on which @what4@ floating-point mode is in use, the result will
    -- be passed to one of three different continuation arguments.
    iFloatGroundEval ::
      forall fi r.
      WFP.FloatInfoRepr fi ->
      WI.SymExpr sym (WFP.SymInterpretedFloatType sym fi) ->
      (BigFloat -> r) ->
      -- ^ Continuation to use if the IEEE floating-point mode is in use.
      (Rational -> r) ->
      -- ^ Continuation to use if the real floating-point mode is in use.
      (forall w. BV.BV w -> r) ->
      -- ^ Continuation to use if the uninterpreted floating-point mode is in
      -- use.
      IO r
    iFloatGroundEval _ e ieeeK realK uninterpK =
      case WI.knownRepr :: WB.FloatModeRepr fm of
        WB.FloatIEEERepr          -> ieeeK <$> WG.groundEval ge e
        WB.FloatRealRepr          -> realK <$> WG.groundEval ge e
        WB.FloatUninterpretedRepr -> uninterpK <$> WG.groundEval ge e
