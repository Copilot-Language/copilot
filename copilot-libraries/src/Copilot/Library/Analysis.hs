-- | Formally analyze streams.
module Copilot.Library.Analysis
    ( triviallyTrue
    , triviallyFalse
    )
  where

-- External imports.
import Prelude hiding (not)

-- Internal imports.
import Copilot.Language      (Stream, forAll, not)
import Copilot.Language.Spec (Prop, Universal)

-- | Property that captures whether a stream is trivially true, meaning that
-- there is no way to make it false.
triviallyTrue :: Stream Bool -> Prop Universal
triviallyTrue = forAll

-- | Property that captures whether a stream is trivially false, meaning that
-- there is no way to make it true.
triviallyFalse :: Stream Bool -> Prop Universal
triviallyFalse = triviallyTrue . not
