-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- |

{-# LANGUAGE UnicodeSyntax #-}

module Copilot.Language
  ( module Data.Int
  , module Data.Word
  , module Copilot.Core
  , module Copilot.Language.Operators.Boolean
  , module Copilot.Language.Operators.Eq
  , module Copilot.Language.Operators.Extern
  , module Copilot.Language.Operators.Mux
  , module Copilot.Language.Operators.Ord
  , module Copilot.Language.Operators.Temporal
  , Stream
  , Trigger
  , Specification
  , constant
  , trigger
  , interpret
  , prettyPrint
  ) where

import Data.Int
import Data.Word
import Copilot.Core (Streamable)
import qualified Copilot.Core.Interpret as I
import qualified Copilot.Core.PrettyPrint as PP
import Copilot.Language.Operators.Boolean
import Copilot.Language.Operators.Eq
import Copilot.Language.Operators.Extern
import Copilot.Language.Operators.Mux
import Copilot.Language.Operators.Ord
import Copilot.Language.Operators.Temporal
import Copilot.Language.Reify
import Copilot.Language.Stream (Stream, Trigger, constant, trigger)

type Specification = [Trigger]

interpret
  ∷ Integer
  → Specification
  → IO ()
interpret i e =
  do
    spec <- reify e
    putStrLn $ I.interpret (fromIntegral i) spec

prettyPrint
  ∷ Specification
  → IO ()
prettyPrint e = fmap PP.prettyPrint (reify e) >>= putStr
