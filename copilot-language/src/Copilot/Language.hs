-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | Main Copilot language export file.
--
-- This is mainly a meta-module that re-exports most definitions in this
-- library. It also provides a default pretty printer that prints a
-- specification to stdout.

{-# LANGUAGE Safe #-}

module Copilot.Language
  ( module Data.Int
  , module Data.Word
  , module Copilot.Core
  , module Copilot.Core.Type
  , module Copilot.Core.Type.Array
  , module Copilot.Language.Error
  , module Copilot.Language.Interpret
  , module Copilot.Language.Operators.Boolean
  , module Copilot.Language.Operators.Cast
  , module Copilot.Language.Operators.Constant
  , module Copilot.Language.Operators.Eq
  , module Copilot.Language.Operators.Extern
  , module Copilot.Language.Operators.Local
  , module Copilot.Language.Operators.Label
  , module Copilot.Language.Operators.Integral
  , module Copilot.Language.Operators.Mux
  , module Copilot.Language.Operators.Ord
  , module Copilot.Language.Operators.Temporal
  , module Copilot.Language.Operators.BitWise
  , module Copilot.Language.Operators.Array
  , module Copilot.Language.Operators.Struct
  , module Copilot.Language.Prelude
  , Spec
  , Stream
  , observer
  , trigger
  , arg
  , prop
  , theorem
  , forall, exists
  , prettyPrint
  ) where

import Data.Int hiding (Int)
import Data.Word
import Copilot.Core (Name, Typed)
import Copilot.Core.Type
import Copilot.Core.Type.Array
import Copilot.Language.Error
import Copilot.Language.Interpret
import Copilot.Language.Operators.Boolean
import Copilot.Language.Operators.Cast
import Copilot.Language.Operators.Constant
import Copilot.Language.Operators.Eq
import Copilot.Language.Operators.Extern
import Copilot.Language.Operators.Integral
import Copilot.Language.Operators.Local
import Copilot.Language.Operators.Label
import Copilot.Language.Operators.Mux
import Copilot.Language.Operators.Ord
import Copilot.Language.Operators.Temporal
import Copilot.Language.Operators.BitWise
import Copilot.Language.Operators.Array
import Copilot.Language.Operators.Struct
import Copilot.Language.Reify
import Copilot.Language.Prelude
import Copilot.Language.Spec
import Copilot.Language.Stream (Stream)
import qualified Copilot.PrettyPrint as PP

-- | Transform a high-level Copilot Language specification into a low-level
-- Copilot Core specification and pretty-print it to stdout.
{-# DEPRECATED prettyPrint "This function is deprecated in Copilot 3.11." #-}
prettyPrint :: Spec -> IO ()
prettyPrint e = fmap PP.prettyPrint (reify e) >>= putStr
