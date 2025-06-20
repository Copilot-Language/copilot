{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds        #-}

-- | An example showing of using @copilot-verifier@ to verify a specification
-- involving structs where individual fields are updated.
module Copilot.Verifier.Examples.ShouldPass.UpdateStruct where

import Language.Copilot
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )

-- | Definition for `Volts`.
data Volts = Volts
  { numVolts :: Field "numVolts" Word32
  , flag     :: Field "flag"     Bool
  }

-- | `Struct` instance for `Volts`.
instance Struct Volts where
  typeName _ = "volts"
  toValues vlts = [ Value Word32 (numVolts vlts)
                  , Value Bool   (flag vlts)
                  ]

-- | `Volts` instance for `Typed`.
instance Typed Volts where
  typeOf = Struct (Volts (Field 0) (Field False))

data Battery = Battery
  { temp  :: Field "temp"  Word32
  , volts :: Field "volts" (Array 10 Volts)
  , other :: Field "other" (Array 10 (Array 5 Word32))
  }

-- | `Battery` instance for `Struct`.
instance Struct Battery where
  typeName _ = "battery"
  toValues battery = [ Value typeOf (temp battery)
                     , Value typeOf (volts battery)
                     , Value typeOf (other battery)
                     ]

-- | `Battery` instance for `Typed`. Note that `undefined` is used as an
-- argument to `Field`. This argument is never used, so `undefined` will never
-- throw an error.
instance Typed Battery where
  typeOf = Struct (Battery (Field 0) (Field undefined) (Field undefined))

spec :: Spec
spec = do
  let battery :: Stream Battery
      battery = extern "battery" Nothing

  -- Update a struct field, then check it for equality.
  trigger "updateTrig"
    ((battery ## temp =$ (+1))#temp == (battery#temp + 1))
    [arg battery]

verifySpec :: Verbosity -> IO ()
verifySpec verb = reify spec >>= verifyWithOptions defaultVerifierOptions{verbosity = verb}
                                                   mkDefaultCSettings [] "updateStruct"
