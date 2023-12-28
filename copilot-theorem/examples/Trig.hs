module Trig where

import Prelude ()

import Copilot.Language
import qualified Copilot.Language.Operators.Propositional as P
import Copilot.Language.Spec

import Copilot.Theorem
import Copilot.Theorem.Prover.SMT

import Copilot.Language.Reify

import Control.Monad (void)

theorem_ a b c = void $ theorem a b c

arith :: Proof Universal
arith = onlyValidity def { debug = False } dReal

arithSat :: Proof Existential
arithSat = onlySat def { debug = False } dReal

(~=) :: Stream Double -> Stream Double -> Stream Bool
a ~= b = abs (a - b) < 0.001

spec = do
  bounds <- prop "bounds" (forAll $ bounds)

  -- dReal/metit fails this one.
  -- theorem "dist_eq"     (forAll $ d1 ~= d2)
  --   $ assume bounds >> arith
  theorem_ "2sin"           (forAll $ (2 * (sin x)) <= 3)
    $ assume bounds >> arith
  theorem_ "sin_cos"        (forAll $ ((sin x) ** 2 + (cos x) ** 2) ~= 1)
    $ assume bounds >> arith
  theorem_ "sin_cos_pi"     ( forAll $ ((sin x) ** 2 + (cos $ x + pi) ** 2) ~= 1)
    $ assume bounds >> arith
  theorem_ "sin_2pi"        ( forAll $ (sin x) ~= (sin $ x + 2 * pi))
    $ assume bounds >> arith
  theorem_ "cos_2pi"        ( forAll $ (cos x) ~= (cos $ x + 2 * pi))
    $ assume bounds >> arith
  theorem_ "sin_eq_cos_pi2" ( forAll $ (sin x) ~= (cos $ x - (pi/2)))
    $ assume bounds >> arith
  theorem_ "x^2_2"          ( forAll $ (x ** 2 + 1) >= x)
    $ assume bounds >> arith
  theorem_ "sqrt_x"         ( forAll $ (x > 2) ==> ((sqrt x) < x))
    $ assume bounds >> arith

  theorem_ "x = y"        (P.not $ forAll $ x == y)
    $ assume bounds >> arithSat
  theorem_ "sin_cos_3"    (P.not $ forAll $ ((sin x) ** 2 + (cos $ x + 3) ** 2) ~= 1)
    $ assume bounds >> arithSat
  theorem_ "sin_pi"       (P.not $ forAll $ (sin x) ~= (sin $ x + pi))
    $ assume bounds >> arithSat
  theorem_ "sin_eq_cos_3" (P.not $ forAll $ (sin x) ~= (cos $ x - (3/2)))
    $ assume bounds >> arithSat
  theorem_ "sin_eq_cos"   (P.not $ forAll $ (sin x) ~= (cos x))
    $ assume bounds >> arithSat

  where
    x = externD "x" Nothing
    y = externD "y" Nothing
    lat1 = externD "lat1" Nothing
    lon1 = externD "lon1" Nothing
    lat2 = externD "lat2" Nothing
    lon2 = externD "lon2" Nothing

    b = 100
    bounds = lat1 < b && lat1 > (-b)
          && lat2 < b && lat2 > (-b)
          && lon1 < b && lon1 > (-b)
          && lon2 < b && lon2 > (-b)
          && x < b && x > (-b)
          && y < b && y > (-b)

    d1 = acos ((sin lat1) * (sin lat2) + (cos lat1) * (cos lat2) * (cos (lon1 - lon2)))
    d2 = 2 * asin (sqrt (
        (sin ((lat1 - lat2)/2)) ** 2 + (cos lat1) * (cos lat2) * ((sin ((lon1 - lon2)/2)) ** 2)))
