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

-- This is ok---indexes just different types.  However, normally, the external
-- array should be given exactly one environment---this will use a different
-- array in different contexts.
extArr0 :: Stream Word32
extArr0 = externArray "arr" x 5 (Just $ repeat [7,8,9,10,11])
extArr1 :: Stream Word32
extArr1 = externArray "arr" y 5 (Just $ repeat [4,4,5,5,6])

-- Both indexes grow to be too big for the array.  The interpreter throws an
-- error.
spec1 :: Spec
spec1 = trigger "trigger" true [ arg extArr0
                               , arg extArr1
                               ]

interp1 :: IO ()
interp1 = interpret 10 spec1

--------------------------------------------------------------------------------

-- Not Ok---saying "arr" is of a different type
extArr2 :: Stream Word16
extArr2 = externArray "arr" y 5 (Just $ repeat [7::Word16,8,9,10,11])

spec2 :: Spec
spec2 = trigger "trigger" true [ arg extArr2
                               , arg extArr1
                               ]

-- Should fail---"arr" given two different types.
interp2 :: IO ()
interp2 = interpret 10 spec2

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
 interp1
 interp2
 interp3
 interp4



