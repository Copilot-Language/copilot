--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs #-}

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
    Bool   -> ShowWit
    Int8   -> ShowWit
    Int16  -> ShowWit
    Int32  -> ShowWit
    Int64  -> ShowWit
    Word8  -> ShowWit
    Word16 -> ShowWit
    Word32 -> ShowWit
    Word64 -> ShowWit
    Float  -> ShowWit
    Double -> ShowWit

--------------------------------------------------------------------------------

showWithType :: Type a -> a -> String
showWithType t x =
  case showWit t of
    ShowWit -> show x

--------------------------------------------------------------------------------
