--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Core.Type.Show
  ( ShowWit (..)
  , showWit
  , showWithType
  ) where

import Copilot.Core.Type
import Copilot.Core.Type.Equality

--------------------------------------------------------------------------------

data ShowWit a = Show a => ShowWit

--------------------------------------------------------------------------------

showWit :: Type a -> ShowWit a
showWit t =
  case t of
    Bool   p -> coerce2 (symm p) ShowWit
    Int8   p -> coerce2 (symm p) ShowWit
    Int16  p -> coerce2 (symm p) ShowWit
    Int32  p -> coerce2 (symm p) ShowWit
    Int64  p -> coerce2 (symm p) ShowWit
    Word8  p -> coerce2 (symm p) ShowWit
    Word16 p -> coerce2 (symm p) ShowWit
    Word32 p -> coerce2 (symm p) ShowWit
    Word64 p -> coerce2 (symm p) ShowWit
    Float  p -> coerce2 (symm p) ShowWit
    Double p -> coerce2 (symm p) ShowWit

--------------------------------------------------------------------------------

showWithType :: Type a -> a -> String
showWithType t x =
  case showWit t of
    ShowWit -> show x

--------------------------------------------------------------------------------
