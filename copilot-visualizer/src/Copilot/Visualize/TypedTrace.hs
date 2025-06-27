{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright : (c) NASA, 2024-2025
-- License   : BSD-style (see the LICENSE file in the distribution)
--
-- Read and modify traces of specifications.
--
-- In the context of this module, a trace refers only to the values of
-- externs, since the state of a spec can be reproduced if the values of
-- externs are known.
module Copilot.Visualize.TypedTrace
    ( extractTrace
    , updateWithTrace
    , Trace(..)
    , UValues(..)
    )
  where

-- External imports
import           Data.List          hiding ((++))
import           Data.Maybe         (fromMaybe)
import qualified Data.Type.Equality as DE
import           Data.Typeable      (Typeable)
import           Prelude            hiding (div, not, (++), (<), (>))

-- External imports: Copilot
import qualified Copilot.Core     as Core
import           Language.Copilot hiding (interpret, typeOf)

-- | Map stream names to their values.
newtype Trace = Trace
  { traceMap :: [ (String, UValues) ]
  }

-- | Existentially typed list of values of a stream.
data UValues = forall a . Typeable a => UValues
  { uvType   :: Core.Type a
  , uvValues :: [ a ]
  }

-- | Extract a trace from a core specification.
extractTrace :: Core.Spec -> Trace
extractTrace spec = Trace $ concat $ concat
  [ fmap extractTraceStream (Core.specStreams spec)
  , fmap extractTraceObserver (Core.specObservers spec)
  , fmap extractTraceTrigger (Core.specTriggers spec)
  ]

-- | Extract the values of a core 'Stream'.
extractTraceStream :: Core.Stream -> [ (String, UValues) ]
extractTraceStream (Core.Stream _id _buf expr _ty) =
  extractTraceExpr expr

-- | Extract the values of a core 'Observer'.
extractTraceObserver :: Core.Observer -> [ (String, UValues) ]
extractTraceObserver (Core.Observer _name expr _ty) =
  extractTraceExpr expr

-- | Extract the values of a core 'Trigger'.
extractTraceTrigger :: Core.Trigger -> [ (String, UValues) ]
extractTraceTrigger (Core.Trigger _name expr args) = concat $
    extractTraceExpr expr
  : fmap extractTraceUExpr args

-- | Extract the values of a core 'Expr'.
extractTraceExpr :: Core.Expr a -> [ (String, UValues) ]
extractTraceExpr (Core.Local _ _ _ expr1 expr2) = concat
  [ extractTraceExpr expr1
  , extractTraceExpr expr2
  ]
extractTraceExpr (Core.ExternVar ty name values) =
  [ (name, UValues ty (fromMaybe [] values)) ]
extractTraceExpr (Core.Op1 _op expr) =
  extractTraceExpr expr
extractTraceExpr (Core.Op2 _op expr1 expr2) = concat
  [ extractTraceExpr expr1
  , extractTraceExpr expr2
  ]
extractTraceExpr (Core.Op3 _op expr1 expr2 expr3) = concat
  [ extractTraceExpr expr1
  , extractTraceExpr expr2
  , extractTraceExpr expr3
  ]
extractTraceExpr (Core.Label _ty _lbl expr) =
  extractTraceExpr expr
extractTraceExpr _ = []

-- | Extract the values of a core 'UExpr'.
extractTraceUExpr :: Core.UExpr -> [ (String, UValues) ]
extractTraceUExpr (Core.UExpr ty expr) = extractTraceExpr expr

-- | Update externs in a spec.
updateWithTrace :: Trace -> Core.Spec -> Core.Spec
updateWithTrace trace spec = spec
  { Core.specStreams =
      fmap (updateWithTraceStream trace) (Core.specStreams spec)
  , Core.specObservers =
      fmap (updateWithTraceObserver trace) (Core.specObservers spec)
  , Core.specTriggers =
      fmap (updateWithTraceTrigger trace) (Core.specTriggers spec)
  }

-- | Update the values of a 'Stream' based on an input trace.
updateWithTraceStream :: Trace -> Core.Stream -> Core.Stream
updateWithTraceStream trace (Core.Stream ident buf expr ty) =
  Core.Stream ident buf (updateWithTraceExpr trace expr) ty

-- | Update the values of an 'Observer' based on an input trace.
updateWithTraceObserver :: Trace -> Core.Observer -> Core.Observer
updateWithTraceObserver trace (Core.Observer name expr ty) =
  Core.Observer name (updateWithTraceExpr trace expr) ty

-- | Update the values of a 'Trigger' based on an input trace.
updateWithTraceTrigger :: Trace -> Core.Trigger -> Core.Trigger
updateWithTraceTrigger trace (Core.Trigger name expr args) =
  Core.Trigger
    name
    (updateWithTraceExpr trace expr)
    (fmap (updateWithTraceUExpr trace) args)

-- | Update the values of an 'Expr' based on an input trace.
updateWithTraceExpr :: Trace -> Core.Expr a -> Core.Expr a
updateWithTraceExpr trace (Core.Local ty1 ty2 name expr1 expr2) =
  Core.Local
    ty1
    ty2
    name
    (updateWithTraceExpr trace expr1)
    (updateWithTraceExpr trace expr2)
updateWithTraceExpr trace (Core.ExternVar ty name values) =
    Core.ExternVar ty name values'
  where
    values' | Just (UValues ty2 vals) <- lookup name (traceMap trace)
            , Just DE.Refl <- DE.testEquality ty ty2
            = Just vals
            | otherwise
            = values
updateWithTraceExpr trace (Core.Op1 op expr) =
  Core.Op1 op (updateWithTraceExpr trace expr)
updateWithTraceExpr trace (Core.Op2 op expr1 expr2) =
  Core.Op2 op
    (updateWithTraceExpr trace expr1)
    (updateWithTraceExpr trace expr2)
updateWithTraceExpr trace (Core.Op3 op expr1 expr2 expr3) =
  Core.Op3 op
    (updateWithTraceExpr trace expr1)
    (updateWithTraceExpr trace expr2)
    (updateWithTraceExpr trace expr3)
updateWithTraceExpr trace (Core.Label ty lbl expr) =
  Core.Label ty lbl (updateWithTraceExpr trace expr)
updateWithTraceExpr trace x = x

-- | Update the values of a 'UExpr' based on an input trace.
updateWithTraceUExpr :: Trace -> Core.UExpr -> Core.UExpr
updateWithTraceUExpr trace (Core.UExpr ty expr) =
  Core.UExpr ty (updateWithTraceExpr trace expr)
