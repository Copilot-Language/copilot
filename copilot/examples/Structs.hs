-- | An example showing how specifications involving structs (in particular,
-- nested structs) are compiled to C using copilot-c99.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Prelude as P
import Control.Monad (void, forM_)

import Language.Copilot
import Copilot.Compile.C99

import GHC.Generics

-- | Definition for `Volts`.
data Volts = Volts
  { numVolts :: Field "numVolts" Word16
  , flag     :: Field "flag"     Bool
  }
  deriving Generic

-- | `Struct` instance for `Volts`.
-- toValues is automatically implemented using Generic
-- Also, no need to manually implement `Typed` is necessary.
instance Struct Volts where
  typeName _ = "volts"

data Battery = Battery
  { temp  :: Field "temp"  Word16
  , volts :: Field "volts" (Array 10 Volts)
  , other :: Field "other" (Array 10 (Array 5 Word32))
  }
  deriving Generic

-- | `Battery` instance for `Struct`.
instance Struct Battery where
  typeName _ = "battery"

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
