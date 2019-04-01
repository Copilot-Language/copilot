--------------------------------------------------------------------------------
-- Copyright © 2019 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- Example showing the use of structs with a vector datatype.

{-# LANGUAGE DataKinds #-}

module Struct where

import Language.Copilot
import Copilot.Compile.C99

import Prelude hiding ((>), (<), div, (++))


data Vec = Vec
  { x :: Field "x" Float
  , y :: Field "y" Float
  }

instance Struct Vec where
  typename _ = "vec"  -- Name of the type in C

  -- Function to translate Vec to list of Value's, order should match struct.
  toValues v = [ Value Float (x v)
               , Value Float (y v)
               ]

-- We need to provide an instance to Typed with a bogus Vec
instance Typed Vec where
  typeOf = Struct (Vec (Field 0) (Field 0))


vecs :: Stream Vec
vecs = [ Vec (Field 1) (Field 2)
       , Vec (Field 12) (Field 8)
       ] ++ vecs


spec = do
  -- Trigger that always executes, splits the vec into seperate args.
  trigger "split" true [arg $ vecs # x, arg $ vecs # y]

main = reify spec >>= compile "struct"
