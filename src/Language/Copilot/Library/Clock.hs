-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Library.Clock
  ( CStream
  , Clock (..)
  , resample
  , spec
  ) where

import Data.Function (on)
import Language.Copilot.Core (Spec, Streamable)
import Language.Copilot.Interface.Operators.Boolean
import Language.Copilot.Interface.Operators.Eq
import Language.Copilot.Interface.Operators.Extern
import Language.Copilot.Interface.Operators.Mux
import Language.Copilot.Interface.Operators.Ord
import Language.Copilot.Interface.Operators.Temporal
import Language.Copilot.Interface.Prelude
import Language.Copilot.Interface.Reify
import Language.Copilot.Interface.Stream
import qualified Prelude as P

newtype CStream ω α = CStream { unCStream :: Stream α }
  deriving
    ( Eq (Stream Bool)
    , Extern β
    , Num
    , Mux (Stream Bool)
    , Ord (Stream Bool)
    , Temporal β
    , P.Eq
    , P.Show
    )

instance Boolean (CStream ω Bool) where
  x && y   = CStream $ on (&&) unCStream x y
  x || y   = CStream $ on (||) unCStream x y
  not      = CStream . not . unCStream
  true     = CStream true
  false    = CStream false
  fromBool = CStream . fromBool

class Clock ω where
  clock ∷ ω → Stream Bool

instance Clock () where
  clock _ = true

resample ∷ (Clock ω1, Clock ω2) ⇒ CStream ω1 α → CStream ω2 α
resample = undefined

spec ∷ ∀ ω α . (Clock ω, Streamable α) ⇒ CStream ω α → IO Spec
spec (CStream x) =
  let
    c = clock (undefined ∷ ω)
    y = mux c x z
    z = [undefined] ++ y
  in
    reify y
