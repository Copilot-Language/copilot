--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Copilot.Core.Type.Show
  ( ShowWit (..)
  , showWit
  , showWithType
  , ShowType(..)
  , showType
  ) where

import Copilot.Core.Type

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

showType :: Type a -> String
showType t =
  case t of
    Bool   -> "Bool"
    Int8   -> "ShowWit"
    Int16  -> "Int16"
    Int32  -> "Int32"
    Int64  -> "Int64"
    Word8  -> "Word8"
    Word16 -> "Word16"
    Word32 -> "Word32"
    Word64 -> "Word64"
    Float  -> "Float"
    Double -> "Double"

--------------------------------------------------------------------------------

-- Are we proving equivalence with a C backend, in which case we want to show
-- Booleans as '0' and '1'.
data ShowType = C | Haskell

--------------------------------------------------------------------------------

showWithType :: ShowType -> Type a -> a -> String
showWithType showT t x =
  case showT of
    C         -> case t of
                   Bool -> if x then "1" else "0"
                   _    -> sw
    Haskell   -> sw
  where
  sw = case showWit t of
         ShowWit -> show x

--------------------------------------------------------------------------------
