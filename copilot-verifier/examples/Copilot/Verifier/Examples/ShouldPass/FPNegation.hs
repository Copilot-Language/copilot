{-# LANGUAGE NoImplicitPrelude #-}

-- | This will not succeed when 'smtFloatMode' is 'FloatUninterpreted', as Clang
-- will turn @input /= 30.0@ below into a more complicated expression that also
-- checks if @30.0@ is NaN or not. This is logically equivalent to the original
-- specification, but an SMT solver cannot conclude this when all of the
-- floating-point operations involved are treated as uninterpreted functions.
--
-- To make this work, we set 'smtFloatMode' to 'FloatIEEE' instead. This works
-- because all of the floating-point operations in the specification below are
-- native to SMT-LIB.
module Copilot.Verifier.Examples.ShouldPass.FPNegation where

import Copilot.Compile.C99 (mkDefaultCSettings)
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )
import Copilot.Verifier.FloatMode (FloatMode(..))
import qualified Copilot.Library.PTLTL as PTLTL
import Language.Copilot

input :: Stream Float
input = extern "input" Nothing

-- | MyProperty
--   @
--   Input is never 30.0
--   @
propMyProperty :: Stream Bool
propMyProperty = PTLTL.alwaysBeen (input /= 30.0)

-- | Clock that increases in one-unit steps.
clock :: Stream Int64
clock = [0] ++ (clock + 1)

-- | First Time Point
ftp :: Stream Bool
ftp = [True] ++ false

pre :: Stream Bool -> Stream Bool
pre = ([False] ++)

tpre :: Stream Bool -> Stream Bool
tpre = ([True] ++)

notPreviousNot :: Stream Bool -> Stream Bool
notPreviousNot = not . PTLTL.previous . not

-- | Complete specification. Calls C handler functions when properties are
-- violated.
spec :: Spec
spec = do
  trigger "handlerMyProperty" (not propMyProperty) []

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions
    (defaultVerifierOptions
      { verbosity = verb
      , smtFloatMode = FloatIEEE
      })
    mkDefaultCSettings [] "fpNegation" spec'
