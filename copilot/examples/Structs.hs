-- | An example showing how specifications involving structs (in particular,
-- nested structs) are compiled to C using copilot-c99.

{-# LANGUAGE DataKinds #-}

module Main where

import qualified Prelude as P
import Control.Monad (void, forM_)

import Language.Copilot
import Copilot.Compile.C99

-- | Definition for `Volts`.
data Volts = Volts
  { numVolts :: Field "numVolts" Word16
  , flag     :: Field "flag"     Bool
  }

-- | `Struct` instance for `Volts`.
instance Struct Volts where
  typename _ = "volts"
  toValues volts = [ Value Word16 (numVolts volts)
                   , Value Bool   (flag volts)
                   ]

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
  typename _ = "battery"
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

  -- Check equality, indexing into nested structs and arrays. Note that this is
  -- trivial by equality.
  trigger "equalitySameIndex"
    ((((battery#volts) .!! 0)#numVolts) == (((battery#volts) .!! 0)#numVolts))
    [arg battery]

  -- Same as previous example, but get a different array index (so should be
  -- false).
  trigger "equalityDifferentIndices"
    ((((battery#other) .!! 2) .!! 3) == (((battery#other) .!! 2) .!! 4))
    [arg battery]

main :: IO ()
main = do
  spec' <- reify spec

  -- Compile the specific to C.
  compile "structs" spec'
