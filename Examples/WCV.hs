{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot
import qualified Copilot.Core as Core
import qualified Copilot.Theorem.What4 as CT
import qualified Prelude as P
import Data.Foldable (forM_)
import Data.Parameterized.Some
import qualified Control.Monad as Monad

dthr, tthr, zthr, tcoathr :: Stream Double
dthr    = extern "dthr" Nothing
tthr    = extern "tthr" Nothing
zthr    = extern "zthr" Nothing
tcoathr = extern "tcoathr" Nothing

type Vect2 = (Stream Double, Stream Double)

--------------------------------
-- Relative velocity/position --
--------------------------------

vx, vy, vz :: Stream Double
vx = extern "relative_velocity_x" Nothing
vy = extern "relative_velocity_y" Nothing
vz = extern "relative_velocity_z" Nothing

v :: (Stream Double, Stream Double)
v = (vx, vy)

sx, sy, sz :: Stream Double
sx = extern "relative_position_x" Nothing
sy = extern "relative_position_y" Nothing
sz = extern "relative_position_z" Nothing

s :: (Stream Double, Stream Double)
s = (sx, sy)

------------------
-- Vector stuff --
------------------

(|*|) :: Vect2 -> Vect2 -> Stream Double
(|*|) (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

sq :: Vect2 -> Stream Double
sq x = x |*| x

norm :: Vect2 -> Stream Double
norm = sqrt . sq

det :: Vect2 -> Vect2 -> Stream Double
det (x1, y1) (x2, y2) = (x1 * y2) - (x2 * y1)

(~=) :: Stream Double -> Stream Double -> Stream Bool
a ~= b = (abs (a - b)) < 0.001

neg :: Vect2 -> Vect2
neg (x, y) = (negate x, negate y)

--------------------
-- Time variables --
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

--------------------------
-- Some tools for times --
--------------------------

tcoa :: Stream Double -> Stream Double -> Stream Double
tcoa sz vz = if (sz * vz) < 0
               then (-sz) / vz
               else -1

dcpa :: Vect2 -> Vect2 -> Stream Double
dcpa s@(sx, sy) v@(vx, vy) = norm (sx + (tcpa s v) * vx, sy + (tcpa s v) * vy)

--------------------------
-- Well clear Violation --
--------------------------

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

spec = do
  Monad.void $ prop "1a" (forall $ (tau s v) ~= (tau (neg s) (neg v)))
  -- Monad.void $ prop "3d" (forall $ (wcv tep s sz v vz)    == (wcv tep (neg s) (-sz) (neg v) (-vz)))

main :: IO ()
main = do
  spec' <- reify spec

  -- Use Z3 to prove the properties.
  results <- CT.prove CT.Z3 spec'

  -- Print the results.
  forM_ results $ \(nm, res) -> do
    putStr $ nm <> ": "
    case res of
      CT.Valid -> putStrLn "valid"
      CT.Invalid -> putStrLn "invalid"
      CT.Unknown -> putStrLn "unknown"
  interpret 10 spec
