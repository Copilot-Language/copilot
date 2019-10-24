--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

-- | Test bad typing on external variables is caught.

{-# LANGUAGE RebindableSyntax #-}

module BadExtVars ( badExtVars ) where

import Language.Copilot

--------------------------------------------------------------------------------

x :: Stream Word8
x = extern "x" (Just $ [0..])


-- Should throw an error
x' :: Stream Word16 -- Bad type!
x' = extern "x" (Just $ [0..])

spec0 :: Spec
spec0 = trigger "trigger" true [ arg x
                               , arg x'
                               ]

-- Should fail: "x" given two different types.
interp0 :: IO ()
interp0 = interpret 10 spec0

--------------------------------------------------------------------------------

y :: Stream Word16
y = extern "y" (Just $ [0..])

--------------------------------------------------------------------------------

-- Not Ok---different number of args.
func0 :: Stream Bool
func0 = externFun "func0" [arg x] (Just $ x < 4)
func1 :: Stream Bool
func1 = externFun "func0" [arg x, arg y] (Just $ (cast x + y) > 10)

spec3 :: Spec
spec3 = trigger "trigger" true [ arg func0
                               , arg func1
                               ]

-- Should fail---different number of args
interp3 :: IO ()
interp3 = interpret 10 spec3

func2 :: Stream Bool
func2 = externFun "func0" [arg y] (Just $ y > 4)

-- Should fail: func0 and func2 say that "func0" have different typed arguments.
spec4 :: Spec
spec4 = trigger "trigger" true [ arg func0, arg func2 ]

interp4 :: IO ()
interp4 =
  interpret 10 spec4

--------------------------------------------------------------------------------

badExtVars :: IO ()
badExtVars = do
 interp0
 interp3
 interp4

main = badExtVars
