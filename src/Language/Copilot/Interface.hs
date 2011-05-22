-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under α Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- |

{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Interface
  ( module Data.Int
  , module Data.Word
  , Stream
  , Streamable
  , interpret
  , (++)
  , (&&), (||)
  , (==), (/=)
  , (<=), (>=), (<), (>)
  , const
  , drop
  , extern
  , not
  , mod
  , mux
  , true
  , false
  ) where

import Data.Int
import Data.Word
import Language.Copilot.Core (Streamable, Op1 (..), Op2 (..), Op3 (..))
import Language.Copilot.Interface.Prelude
import Language.Copilot.Interface.Reify (reify)
import Language.Copilot.Interface.Stream (Stream (..))
import qualified Language.Copilot.Core.Interpret as I
import qualified Prelude as P

infixr 3 ++

(++) ∷ Streamable α ⇒ [α] → Stream α → Stream α
(++) = Append

const ∷ Streamable α ⇒ α → Stream α
const = Const

drop ∷ Streamable α ⇒ Int → Stream α → Stream α
drop = Drop

extern ∷ Streamable α ⇒ String → Stream α
extern = Extern

not ∷ Stream Bool → Stream Bool
not = Op1 Not

(&&) ∷ Stream Bool → Stream Bool → Stream Bool
(&&) = Op2 (:&&:)

(||) ∷ Stream Bool → Stream Bool → Stream Bool
(||) = Op2 (:||:)

(==) ∷ (Streamable α, P.Eq α) ⇒ Stream α → Stream α → Stream Bool
(==) = Op2 (:==:)

(/=) ∷ (Streamable α, P.Eq α) ⇒ Stream α → Stream α → Stream Bool
(/=) = Op2 (:/=:)

(<=) ∷ (Streamable α, P.Ord α) ⇒ Stream α → Stream α → Stream Bool
(<=) = Op2 (:<=:)

(>=) ∷ (Streamable α, P.Ord α) ⇒ Stream α → Stream α → Stream Bool
(>=) = Op2 (:>=:)

(<) ∷ (Streamable α, P.Ord α) ⇒ Stream α → Stream α → Stream Bool
(<) = Op2 (:<:)

(>) ∷ (Streamable α, P.Ord α) ⇒ Stream α → Stream α → Stream Bool
(>) = Op2 (:>:)

mod ∷ (Streamable α, Integral α) ⇒ Stream α → Stream α → Stream α
mod = Op2 Mod

mux ∷ Streamable α ⇒ Stream Bool → Stream α → Stream α → Stream α
mux = Op3 Mux

true ∷ Stream Bool
true = Const True

false ∷ Stream Bool
false = Const False

interpret
  ∷ Streamable α
  ⇒ Integer
  → Stream α
  → IO ()
interpret i e =
  do
    sp <- reify e
    let xs = take (fromInteger i) $ I.interpret sp
    print xs
