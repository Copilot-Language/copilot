-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under α Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- |

{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Interface
  ( module Data.Int
  , module Data.Word
  , module Language.Copilot.Core
  , module Language.Copilot.Interface.Operators.Boolean
  , module Language.Copilot.Interface.Operators.Eq
  , module Language.Copilot.Interface.Operators.Extern
  , module Language.Copilot.Interface.Operators.Mux
  , module Language.Copilot.Interface.Operators.Ord
  , module Language.Copilot.Interface.Operators.Temporal
  , Stream
  , constant
--  , interpret
  , prettyPrint
  ) where

import Data.Int
import Data.Word
import Language.Copilot.Core (Streamable)
import Language.Copilot.Interface.Operators.Boolean
import Language.Copilot.Interface.Operators.Eq
import Language.Copilot.Interface.Operators.Extern
import Language.Copilot.Interface.Operators.Mux
import Language.Copilot.Interface.Operators.Ord
import Language.Copilot.Interface.Operators.Temporal
import Language.Copilot.Interface.Reify
import Language.Copilot.Interface.Stream (Stream, constant)

--import qualified Language.Copilot.Core.Interpret as I
import qualified Language.Copilot.Core.PrettyPrint as PP

{-
interpret
  ∷ Streamable α
  ⇒ Integer
  → Stream α
  → IO ()
interpret i e =
  do
    spec <- reify e
    let xs = take (fromInteger i) $ I.interpret spec
    print xs
-}

prettyPrint
  ∷ Streamable α
  ⇒ Stream α
  → IO ()
prettyPrint e = fmap PP.prettyPrint (reify e) >>= putStr
