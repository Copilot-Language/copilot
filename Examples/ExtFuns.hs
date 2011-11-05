--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Example in sampling external functions.

{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot 
import qualified Copilot.Compile.C99 as C
--------------------------------------------------------------------------------

nats :: Stream Word16
nats = [0] ++ nats + 1

sample0 :: Stream Word16
sample0 = externFun "func0" [funArg $ externW8 "x", funArg nats]

sample1 :: Stream Bool
sample1 = externFun "func1" []

spec :: Spec
spec =
  trigger "trigger" true [ arg sample0, arg sample1 ]

main :: IO ()
main = 
  reify spec >>= C.compile C.defaultParams 

--------------------------------------------------------------------------------
