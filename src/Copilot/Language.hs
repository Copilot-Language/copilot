--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Main Copilot language export file.

{-# LANGUAGE Safe #-}

module Copilot.Language
  ( module Data.Int
  , module Data.Word
  , module Copilot.Core
  , module Copilot.Language.Error
  , module Copilot.Language.Interpret
  , module Copilot.Language.Operators.Boolean
  , module Copilot.Language.Operators.Cast
  , module Copilot.Language.Operators.Constant
  , module Copilot.Language.Operators.Eq
  , module Copilot.Language.Operators.Extern
  , module Copilot.Language.Operators.Local
  , module Copilot.Language.Operators.Integral
  , module Copilot.Language.Operators.Mux
  , module Copilot.Language.Operators.Ord
  , module Copilot.Language.Operators.Temporal
  , module Copilot.Language.Operators.BitWise
  , Spec
  , Stream
  , observer
  , trigger
  , arg
  , prettyPrint
  ) where

import Data.Int hiding (Int)
import Data.Word
import Copilot.Core (Name, Typed)
import qualified Copilot.Core.PrettyPrint as PP
import Copilot.Language.Error
import Copilot.Language.Interpret
import Copilot.Language.Operators.Boolean
import Copilot.Language.Operators.Cast
import Copilot.Language.Operators.Constant
import Copilot.Language.Operators.Eq
import Copilot.Language.Operators.Extern
import Copilot.Language.Operators.Integral
import Copilot.Language.Operators.Local
import Copilot.Language.Operators.Mux
import Copilot.Language.Operators.Ord
import Copilot.Language.Operators.Temporal
import Copilot.Language.Operators.BitWise
import Copilot.Language.Reify
import Copilot.Language.Spec (Spec, trigger, arg, observer)
import Copilot.Language.Stream (Stream)

--------------------------------------------------------------------------------

prettyPrint :: Spec -> IO ()
prettyPrint e = fmap PP.prettyPrint (reify e) >>= putStr

--------------------------------------------------------------------------------
