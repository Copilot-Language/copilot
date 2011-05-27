-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
-- CoPilot is licensed under a Creative Commons Attribution 3.0 Unported License.
-- See http://creativecommons.org/licenses/by/3.0 for license terms.

-- Dynamic StableNames without phantom type.

{-# LANGUAGE UnicodeSyntax #-}

module Language.Copilot.Interface.Reify.DynStableName
  ( StableName
  , makeStableName
  , hashStableName
  ) where

import qualified System.Mem.StableName as S
import Unsafe.Coerce (unsafeCoerce)

newtype StableName = StableName (S.StableName ())

instance Eq StableName where
  (StableName x) == (StableName y) = x == y

makeStableName ∷ α → IO StableName
makeStableName a = do
  sn <- S.makeStableName a
  return $ StableName (unsafeCoerce sn)

hashStableName ∷ StableName → Int
hashStableName (StableName sn) = S.hashStableName sn
