-- | This example shows an implementation of the Well-Clear Violation
-- algorithm, it follows the implementation described in 'Analysis of
-- Well-Clear Bounday Models for the Integration of UAS in the NAS',
-- https://ntrs.nasa.gov/citations/20140010078.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Copilot.Verifier.Examples.ShouldPass.WCV where

import Language.Copilot
-- import qualified Copilot.Theorem.What4 as CT
import Copilot.Compile.C99
import Copilot.Verifier ( Verbosity, VerifierOptions(..)
                        , defaultVerifierOptions, verifyWithOptions )

-- import Data.Foldable (forM_)
import qualified Control.Monad as Monad


-- | `dthr` is the horizontal distance threshold.
dthr :: Stream Double
dthr = extern "dthr" Nothing

-- | `tthr` is the horizontal time threshold.
tthr :: Stream Double
tthr = extern "tthr" Nothing

-- | `zthr` is the vertical distance / altitude threshold.
zthr :: Stream Double
zthr = extern "zthr" Nothing

-- | `tcoathr` is the vertical time threshold.
tcoathr :: Stream Double
tcoathr = extern "tcoathr" Nothing

type Vect2 = (Stream Double, Stream Double)


------------------
-- The following section contains basic libraries for working with vectors.
------------------

-- | Multiply two Vectors.
(|*|) :: Vect2 -> Vect2 -> Stream Double
(|*|) (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

-- | Calculate the square of a vector.
sq :: Vect2 -> Stream Double
sq x = x |*| x

-- | Calculate the length of a vector.
norm :: Vect2 -> Stream Double
norm = sqrt . sq

-- | Calculate the determinant of two vectors.
det :: Vect2 -> Vect2 -> Stream Double
det (x1, y1) (x2, y2) = (x1 * y2) - (x2 * y1)

-- | Compare two vectors, taking into account the small error that is
-- introduced by the usage of `Double`s.
(~=) :: Stream Double -> Stream Double -> Stream Bool
a ~= b = (abs (a - b)) < 0.001

-- | Negate a vector.
neg :: Vect2 -> Vect2
neg (x, y) = (negate x, negate y)


--------------------
-- From here on the algorithm, as described by the paper mentioned on the top
-- of this file, is implemented. Please refer to the paper for details.
--------------------

tau :: Vect2 -> Vect2 -> Stream Double
tau s v = if s |*| v < 0
            then (-(sq s)) / (s |*| v)
            else -1

tcpa :: Vect2 -> Vect2 -> Stream Double
tcpa s v@(vx, vy) = if vx ~= 0 && vy ~= 0
                      then 0
                      else -(s |*| v)/(sq v)

taumod :: Vect2 -> Vect2 -> Stream Double
taumod s v = if s |*| v < 0
               then (dthr * dthr - (sq s))/(s |*| v)
               else -1

tep :: Vect2 -> Vect2 -> Stream Double
tep s v = if (s |*| v < 0) && ((delta s v dthr) >= 0)
            then theta s v dthr (-1)
            else -1

delta :: Vect2 -> Vect2 -> Stream Double -> Stream Double
delta s v d = (d*d) * (sq v) - ((det s v)*(det s v))
-- Here the formula says : (s . orth v)^2 which is the same as det(s,v)^2

theta :: Vect2 -> Vect2 -> Stream Double -> Stream Double -> Stream Double
theta s v d e = (-(s |*| v) + e * (sqrt $ delta s v d)) / (sq v)


tcoa :: Stream Double -> Stream Double -> Stream Double
tcoa sz vz = if (sz * vz) < 0
               then (-sz) / vz
               else -1

dcpa :: Vect2 -> Vect2 -> Stream Double
dcpa s@(sx, sy) v@(vx, vy) = norm (sx + (tcpa s v) * vx, sy + (tcpa s v) * vy)


--------------------------
-- Well clear Violation --
--------------------------

-- | Determines if the well clear property is violated or not.
wcv :: (Vect2 -> Vect2 -> Stream Double) ->
       Vect2 -> Stream Double ->
       Vect2 -> Stream Double ->
       Stream Bool
wcv tvar s sz v vz = (horizontalWCV tvar s v) && (verticalWCV sz vz)

verticalWCV :: Stream Double -> Stream Double -> Stream Bool
verticalWCV sz vz =
  ((abs $ sz) <= zthr) ||
  (0 <= (tcoa sz vz) && (tcoa sz vz) <= tcoathr)

horizontalWCV :: (Vect2 -> Vect2 -> Stream Double) -> Vect2 -> Vect2 -> Stream Bool
horizontalWCV tvar s v =
  (norm s <= dthr) ||
  (((dcpa s v) <= dthr) && (0 <= (tvar s v)) && ((tvar s v) <= tthr))

spec :: Spec
spec = do
  -- External streams for relative position and velocity.
  let -- The relative x velocity between ownship and the intruder.
      vx :: Stream Double
      vx = extern "relative_velocity_x" Nothing

      -- The relative y velocity between ownship and the intruder.
      vy :: Stream Double
      vy = extern "relative_velocity_y" Nothing

      -- The relative z velocity between ownship and the intruder.
      vz :: Stream Double
      vz = extern "relative_velocity_z" Nothing

      -- The relative velocity as a 2D vector.
      v :: (Stream Double, Stream Double)
      v = (vx, vy)


      -- The relative x position between ownship and the intruder.
      sx :: Stream Double
      sx = extern "relative_position_x" Nothing

      -- The relative y position between ownship and the intruder.
      sy :: Stream Double
      sy = extern "relative_position_y" Nothing

      -- The relative z position between ownship and the intruder.
      sz :: Stream Double
      sz = extern "relative_position_z" Nothing

      -- The relative position as a 2D vector.
      s :: (Stream Double, Stream Double)
      s = (sx, sy)
  Monad.void $ prop "1a" (forAll $ (tau s v) ~= (tau (neg s) (neg v)))
  -- Monad.void $ prop "3d" (forAll $ (wcv tep s sz v vz)    == (wcv tep (neg s) (-sz) (neg v) (-vz)))
  trigger "well_clear_violation" (wcv tep s sz v vz) []

verifySpec :: Verbosity -> IO ()
verifySpec verb = do
  spec' <- reify spec
  verifyWithOptions defaultVerifierOptions{verbosity = verb}
                    mkDefaultCSettings [] "wcv" spec'

{-
  -- Use Z3 to prove the properties.
  results <- CT.prove CT.Z3 spec'

  -- Print the results.
  forM_ results $ \(nm, res) -> do
    putStr $ nm <> ": "
    case res of
      CT.Valid -> putStrLn "valid"
      CT.Invalid -> putStrLn "invalid"
      CT.Unknown -> putStrLn "unknown"
-}
