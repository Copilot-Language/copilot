--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

module Copilot.Language
  ( module Data.Int
  , module Data.Word
  , module Copilot.Core
  , module Copilot.Language.Interpret
  , module Copilot.Language.Operators.Boolean
  , module Copilot.Language.Operators.Eq
  , module Copilot.Language.Operators.Extern
  , module Copilot.Language.Operators.Integral
  , module Copilot.Language.Operators.Mux
  , module Copilot.Language.Operators.Ord
  , module Copilot.Language.Operators.Temporal
  , Stream
  , Copilot
  , constant
  , trigger
  , triggerArg
  , prettyPrint
  ) where

import Data.Int
import Data.Word
import Copilot.Core (Name, Typed)
import qualified Copilot.Core.PrettyPrint as PP
import Copilot.Language.Interpret
import Copilot.Language.Operators.Boolean
import Copilot.Language.Operators.Eq
import Copilot.Language.Operators.Extern
import Copilot.Language.Operators.Integral
import Copilot.Language.Operators.Mux
import Copilot.Language.Operators.Ord
import Copilot.Language.Operators.Temporal
import Copilot.Language.Reify
import Copilot.Language.Stream (Stream, Copilot, Trigger, constant, trigger, triggerArg, getList)

--------------------------------------------------------------------------------

prettyPrint
  :: Copilot
  -> IO ()
prettyPrint e = fmap PP.prettyPrint (reify $ getList e) >>= putStr

--------------------------------------------------------------------------------
