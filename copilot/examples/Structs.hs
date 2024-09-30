-- | An example showing how specifications involving structs (in particular,
-- nested structs) are interpreted and how they are compiled to C using
-- copilot-c99.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Prelude as P
import Control.Monad (void, forM_)
import Data.Proxy (Proxy(..))
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import GHC.TypeLits (sameSymbol)

import Language.Copilot
import Copilot.Compile.C99

-- | Definition for `Volts`.
data Volts = Volts
  { numVolts :: Field "numVolts" Word16
  , flag     :: Field "flag"     Bool
  }

-- | `Struct` instance for `Volts`.
instance Struct Volts where
  typeName _ = "volts"
  toValues volts = [ Value Word16 (numVolts volts)
                   , Value Bool   (flag volts)
                   ]
  -- In order to run struct updates (as used in the "equalityStructUpdate"
  -- trigger below) in the Copilot interpreter, we must implement the
  -- `updateField` method. To do so, we must check to see if the supplied
  -- `Value` has a `Field` with the same name and type as a field in `Volts`.
  updateField volts (Value fieldTy (field :: Field fieldName a))
      -- For each field in `Volts`, we must:
      --
      -- 1. Check that the field names match using `sameSymbol`. Here,
      --    "numVolts" is the expected name, and `fieldName` is the actual name
      --    that is supplied as an argument to `updateField`. If the check
      --    succeeds, then the `sameSymbol` function will return `Just p`, where
      --    `p` is proof that the two names are the same.
    | Just Refl <- sameSymbol (Proxy @"numVolts") (Proxy @fieldName)
      -- 2. Check that the field types match using `testEquality`. Here,
      --    `Word16` is the expected type, and `fieldTy` is the actual type that
      --    is supplied as an argument. Again, `testEquality` will return `Just
      --    p` (where `p` is a proof) if the two are the same.
    , Just Refl <- testEquality Word16 fieldTy
      -- 3. If both of the checks above succeed, then we can update the field's
      --    value using a record update.
    = volts { numVolts = field }

      -- It is possible that the `Value` passed as an argument could correspond
      -- to any of the fields in `Volts`, so we must repeat this process for
      -- the `flag` field as well.
    | Just Refl <- sameSymbol (Proxy @fieldName) (Proxy @"flag")
    , Just Refl <- testEquality fieldTy Bool
    = volts { flag = field }

      -- If the supplied `Value` does not correspond to any field in `Volts`,
      -- then something went wrong in the Copilot interpreter. This case reports
      -- this as an error.
    | otherwise
    = error $ "Unexpected field: " P.++ show field

-- | `Volts` instance for `Typed`.
instance Typed Volts where
  typeOf = Struct (Volts (Field 0) (Field False))

data Battery = Battery
  { temp  :: Field "temp"  Word16
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
  -- We implement `updateField` similarly to how we implement it in the
  -- `Struct Volts` instance above.
  updateField battery (Value fieldTy (field :: Field fieldName a))
    | Just Refl <- sameSymbol (Proxy @fieldName) (Proxy @"temp")
    , Just Refl <- testEquality fieldTy Word16
    = battery { temp = field }

    | Just Refl <- sameSymbol (Proxy @fieldName) (Proxy @"volts")
      -- Note that writing out the full `Type` for `Volts` is somewhat verbose,
      -- so we make use of the `Typed Volts` instance and write `typeOf @Volts`
      -- instead.
    , Just Refl <- testEquality fieldTy (Array @10 (typeOf @Volts))
    = battery { volts = field }

    | Just Refl <- sameSymbol (Proxy @fieldName) (Proxy @"other")
    , Just Refl <- testEquality fieldTy (Array @10 (Array @5 Word32))
    = battery { other = field }

    | otherwise
    = error $ "Unexpected field: " P.++ show field

-- | `Battery` instance for `Typed`. Note that `undefined` is used as an
-- argument to `Field`. This argument is never used, so `undefined` will never
-- throw an error.
instance Typed Battery where
  typeOf = Struct (Battery (Field 0) (Field undefined) (Field undefined))

spec :: Spec
spec = do
  let voltsValue :: Volts
      voltsValue =
        Volts
          { numVolts = Field 42
          , flag = Field True
          }

      batteryValue :: Battery
      batteryValue =
        Battery
          { temp = Field 0
          , volts = Field (array (replicate 10 voltsValue))
          , other = Field (array (replicate 10 (array (replicate 5 0))))
          }

      battery :: Stream Battery
      battery = extern "battery" (Just [batteryValue])

  -- Check equality, indexing into nested structs and arrays. Note that this is
  -- trivial by equality.
  trigger "equalitySameIndex"
    ((((battery#volts) ! 0)#numVolts) == (((battery#volts) ! 0)#numVolts))
    [arg battery]

  -- Same as previous example, but get a different array index (so should be
  -- false).
  trigger "equalityDifferentIndices"
    ((((battery#other) ! 2) ! 3) == (((battery#other) ! 2) ! 4))
    [arg battery]

  -- Update a struct field, then check it for equality.
  let batteryTemp1, batteryTemp2 :: Stream Word16
      batteryTemp1 = (battery ## temp =$ (+1))#temp
      batteryTemp2 = battery#temp + 1
  trigger "equalityStructUpdate"
    (batteryTemp1 == batteryTemp2)
    [arg battery, arg batteryTemp1, arg batteryTemp2]

main :: IO ()
main = do
  -- Run the specification using the Copilot interpreter.
  interpret 1 spec

  -- Compile the specification to C.
  spec' <- reify spec
  compile "structs" spec'
