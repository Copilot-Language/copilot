{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Copilot.Verifier.FloatMode
  ( FloatMode(..)
  , withFloatMode
  , withInterpretedFloatExprBuilder
  ) where

import qualified What4.Expr.Builder as W4
import qualified What4.InterpretedFloatingPoint as W4

-- | How the verifier should interpret floating-point operations.
data FloatMode
  = -- | Floating-point values are treated as bitvectors of the appropriate
    -- width, and all operations on them are translated as uninterpreted
    -- functions. This is the verifier's default interpretation, and it is what
    -- allows the verifier to perform any reasoning at all over floating-point
    -- operations that are not native to SMT-LIB (@sin@, @cos@, @tan@, etc.).
    FloatUninterpreted

  | -- | Floating-point values are treated as bit-precise IEEE-754 floats. This
    -- interpretation can perform deeper reasoning about floating-point
    -- operations that are native to SMT-LIB, but at the expense of causing the
    -- verifier to throw an error if it encounters operations that are not
    -- native to SMT-LIB (@sin@, @cos@, @tan@, etc.). Use at your own risk.
    FloatIEEE
  deriving Show

-- | Convert a 'FloatMode' into a What4 'W4.FloatModeRepr' and pass it to a
-- continuation.
withFloatMode ::
  FloatMode ->
  (forall fm. W4.FloatModeRepr fm -> r) ->
  r
withFloatMode fm k =
  case fm of
    FloatUninterpreted ->
      k W4.FloatUninterpretedRepr
    FloatIEEE ->
      k W4.FloatIEEERepr

-- | Match on a 'W4.FloatModeRepr', compute evidence that it gives rise to an
-- instance of 'W4.IsInterpretedFloatExprBuilder', and pass the evidence to a
-- continutation.
withInterpretedFloatExprBuilder ::
  W4.ExprBuilder t st (W4.Flags fm) ->
  W4.FloatModeRepr fm ->
  (W4.IsInterpretedFloatExprBuilder (W4.ExprBuilder t st (W4.Flags fm)) => r) ->
  r
withInterpretedFloatExprBuilder _sym fm k =
  case fm of
    W4.FloatUninterpretedRepr -> k
    W4.FloatIEEERepr -> k
    W4.FloatRealRepr -> k
