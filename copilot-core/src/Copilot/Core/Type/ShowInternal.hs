{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE Safe                      #-}

-- |
-- Copyright: (c) 2011 National Institute of Aerospace / Galois, Inc.
--
-- Show Copilot Core types and typed values.
module Copilot.Core.Type.ShowInternal
    ( showWithType
    , ShowType (..)
    , showType
    )
  where

-- Internal imports
import Copilot.Core.Type (Type (..))

-- | Target language for showing a typed value. Used to adapt the
-- representation of booleans.
data ShowType = C | Haskell

-- | Show a value. The representation depends on the type and the target
-- language. Booleans are represented differently depending on the backend.
showWithType :: ShowType -> Type a -> a -> String
showWithType showT t x =
    case showT of
      C       -> case t of
                   Bool -> if x then "1" else "0"
                   _    -> sw
      Haskell -> case t of
                   Bool -> if x then "true" else "false"
                   _    -> sw
  where
    sw = case showWit t of
           ShowWit -> show x

-- | Show Copilot Core type.
showType :: Type a -> String
showType t =
  case t of
    Bool     -> "Bool"
    Int8     -> "Int8"
    Int16    -> "Int16"
    Int32    -> "Int32"
    Int64    -> "Int64"
    Word8    -> "Word8"
    Word16   -> "Word16"
    Word32   -> "Word32"
    Word64   -> "Word64"
    Float    -> "Float"
    Double   -> "Double"
    Array t  -> "Array " ++ showType t
    Struct t -> "Struct"

-- * Auxiliary show instance

-- | Witness datatype for showing a value, used by 'showWithType'.
data ShowWit a = Show a => ShowWit

-- | Turn a type into a show witness.
showWit :: Type a -> ShowWit a
showWit t =
  case t of
    Bool     -> ShowWit
    Int8     -> ShowWit
    Int16    -> ShowWit
    Int32    -> ShowWit
    Int64    -> ShowWit
    Word8    -> ShowWit
    Word16   -> ShowWit
    Word32   -> ShowWit
    Word64   -> ShowWit
    Float    -> ShowWit
    Double   -> ShowWit
    Array t  -> ShowWit
    Struct t -> ShowWit
