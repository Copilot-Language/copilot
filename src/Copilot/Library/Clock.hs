--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Copilot.Library.Clock
  ( CStream
  , Clock (..)
  , resample
  ) where

import Data.Function (on)
import Copilot.Core (Typed)
import Copilot.Language.Operators.Boolean
import Copilot.Language.Operators.Eq
import Copilot.Language.Operators.Extern
import Copilot.Language.Operators.Mux
import Copilot.Language.Operators.Ord
import Copilot.Language.Operators.Temporal
import Copilot.Language.Prelude
--import Copilot.Language.Reify
import Copilot.Language.Stream
import qualified Prelude as P

--------------------------------------------------------------------------------

newtype CStream ω a = CStream { unCStream :: Stream a }
  deriving
    ( Num
    , P.Eq
    , P.Show
    )

--------------------------------------------------------------------------------

instance Boolean (CStream ω Bool) where
  x && y    = CStream $ on (&&) unCStream x y
  x || y    = CStream $ on (||) unCStream x y
  not       = CStream . not . unCStream
  true      = CStream true
  false     = CStream false
  fromBool  = CStream . fromBool

--------------------------------------------------------------------------------

instance (P.Eq a, Typed a, Show a) => Eq (CStream ω a) (CStream ω Bool) where
  x == y    = CStream $ on (==) unCStream x y
  x /= y    = CStream $ on (==) unCStream x y

--------------------------------------------------------------------------------

instance (P.Ord a, Typed a, Show a) => Ord (CStream ω a) (CStream ω Bool) where
  x <= y    = CStream $ on (<=) unCStream x y
  x >= y    = CStream $ on (>=) unCStream x y
  x <  y    = CStream $ on (<)  unCStream x y
  x >  y    = CStream $ on (>)  unCStream x y

--------------------------------------------------------------------------------

instance (Typed a, Show a) => Mux (CStream ω a) (CStream ω Bool) where
  mux v x y = CStream $ mux (unCStream v) (unCStream x) (unCStream y)

--------------------------------------------------------------------------------

instance (Typed b, Show b) => Temporal (CStream ω) b where
  xs ++ y   = CStream $ (++) xs (unCStream y)
  drop i x  = CStream $ drop i (unCStream x)

--------------------------------------------------------------------------------

instance Extern (CStream ω) where
  extern    = CStream . extern

--------------------------------------------------------------------------------

class Clock ω where
  type Master ω :: *
  clock :: (Master ω ~ ω0, Clock ω0) => ω -> CStream ω0 Bool

--------------------------------------------------------------------------------

instance Clock () where
  type Master () = ()
  clock _ = true

--------------------------------------------------------------------------------

resample :: (Clock ω1, Clock ω2) => CStream ω1 a -> CStream ω2 a
resample = undefined

--------------------------------------------------------------------------------

{-
spec :: forall ω a . (Clock ω, Streamable a) => CStream ω a -> IO Spec
spec (CStream x) =
  let
    c = clock (undefined :: ω)
    y = mux c x z
    z = [undefined] ++ y
  in
    reify y
-}

--------------------------------------------------------------------------------