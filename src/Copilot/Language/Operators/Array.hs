{-# LANGUAGE Safe #-}

{-# LANGUAGE TypeFamilies #-}

module Copilot.Language.Operators.Array
  ( (.!!)
  ) where

import Copilot.Core             ( Typed
                                , Op2 (Index)
                                , typeOf
                                , Array
                                , InnerType
                                , Flatten)
import Copilot.Language.Stream  (Stream (..))

import Data.Word                (Word32)
import GHC.TypeLits             (KnownNat)

--------------------------------------------------------------------------------

(.!!) :: ( KnownNat n
         , t' ~ InnerType t
         , Flatten t t'
         , Typed t
         , Typed t'
         ) => Stream (Array n t) -> Stream Word32 -> Stream t
arr .!! n = Op2 (Index typeOf) arr n

--------------------------------------------------------------------------------
