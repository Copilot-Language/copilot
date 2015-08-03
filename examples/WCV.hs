{-# LANGUAGE FlexibleInstances #-}

module WCV where

import Copilot.Language.Reify
import Copilot.Language

import Prelude ()

import Copilot.Kind.Light.Prover
import Copilot.Kind.Prover

dthr    = externD "dthr" Nothing
tthr    = externD "tthr" Nothing
zthr    = externD "zthr" Nothing
tcoathr = externD "tcoathr" Nothing

type Vect2 = (Stream Double, Stream Double)

--------------------------------
-- Relative velocity/position --
--------------------------------

vx = externD "relative_velocity_x" Nothing
vy = externD "relative_velocity_y" Nothing
vz = externD "relative_velocity_z" Nothing
v = (vx, vy)

sx = externD "relative_position_x" Nothing
sy = externD "relative_position_y" Nothing
sz = externD "relative_position_z" Nothing
s = (sx, sy)

------------------
-- Vector stuff --
------------------

(<*>) :: Vect2 -> Vect2 -> Stream Double
(<*>) (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

sq :: Vect2 -> Stream Double
sq x = x <*> x

norm :: Vect2 -> Stream Double
norm = sqrt . sq

det :: Vect2 -> Vect2 -> Stream Double
det (x1, y1) (x2, y2) = (x1 * y2) - (x2 * y1)

(~=) :: Stream Double -> Stream Double -> Stream Bool
a ~= b = (abs (a - b)) < epsilon
epsilon = 0.001

neg :: Vect2 -> Vect2
neg (x, y) = (negate x, negate y)

--------------------
-- Time variables --
--------------------

tau :: Vect2 -> Vect2 -> Stream Double
tau s v = mux (s <*> v < 0) ((-(sq s)) / (s <*> v)) (-1)

tcpa :: Vect2 -> Vect2 -> Stream Double
tcpa s v@(vx, vy) = mux (vx ~= 0 && vy ~= 0) 0 (-(s <*> v)/(sq v))

taumod :: Vect2 -> Vect2 -> Stream Double
taumod s v = mux (s <*> v < 0) ((dthr * dthr - (sq s))/(s <*> v)) (-1)

tep :: Vect2 -> Vect2 -> Stream Double
tep s v = mux ((s <*> v < 0) && ((delta s v dthr) >= 0))
  (theta s v dthr (-1))
  (-1)

delta :: Vect2 -> Vect2 -> Stream Double -> Stream Double
delta s v d = d**2 * (sq v) - (det s v)**2
-- Here the formula says : (s . orth v)^2 which is the same as det(s,v)^2

theta :: Vect2 -> Vect2 -> Stream Double -> Stream Double -> Stream Double
theta s v d e = (-(s <*> v) + e * (sqrt $ delta s v d)) / (sq v)

--------------------------
-- Some tools for times --
--------------------------

tcoa :: Stream Double -> Stream Double -> Stream Double
tcoa sz vz = mux ((sz * vz) < 0) ((-sz) / vz) (-1)

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

--------------------
-- Locally convex --
--------------------

t1 = externD "t1" Nothing
t2 = externD "t2" Nothing
t3 = externD "t3" Nothing

lc :: (Vect2 -> Vect2 -> Stream Double) -> Stream Bool
lc tvar = (0 <= t1 && t1 <= t2 && t2 <= t3)
   ==> not ((wcv tvar (sx + t1*vx, sy + t1*vy) (sz + t1*vz) v vz)
   &&  (not $ wcv tvar (sx + t2*vx, sy + t2*vy) (sz + t2*vz) v vz)
   &&  (wcv tvar (sx + t3*vx, sy + t3*vy) (sz + t3*vz) v vz))

----------
-- Spec --
----------

spec :: Spec
spec = do
  -------------------------
  -- Horizontal symmetry --
  -------------------------
  prop "1a" $ (tau s v)      ~= (tau (neg s) (neg v))
  prop "1b" $ (tcpa s v)   ~= (tcpa (neg s) (neg v))
  prop "1c" $ (taumod s v) ~= (taumod (neg s) (neg v))
  prop "1d" $ (tep s v)    ~= (tep (neg s) (neg v))

  -------------------------
  -- Horizontal ordering --
  -------------------------
  prop "2a" $ ((s <*> v) < 0 && (norm s) > dthr && (dcpa s v) <= dthr)
    ==> ((tep s v) <= (taumod s v))
  prop "2b" $ ((s <*> v) < 0 && (norm s) > dthr && (dcpa s v) <= dthr)
    ==> ((taumod s v) <= (tcpa s v))
  prop "2c" $ ((s <*> v) < 0 && (norm s) > dthr && (dcpa s v) <= dthr)
    ==> ((tcpa s v) <= (tau s v))

  --------------
  -- Symmetry --
  --------------
  prop "3a" $ (wcv tau s sz v vz) == (wcv tau (neg s) (-sz) (neg v) (-vz))
  prop "3b" $ (wcv tcpa s sz v vz) == (wcv tcpa (neg s) (-sz) (neg v) (-vz))
  prop "3c" $ (wcv taumod s sz v vz) == (wcv taumod (neg s) (-sz) (neg v) (-vz))
  prop "3d" $ (wcv tep s sz v vz) == (wcv tep (neg s) (-sz) (neg v) (-vz))

  ---------------
  -- Inclusion --
  ---------------
  prop "4i"   $ (wcv tau s sz v vz)    ==> (wcv tcpa s sz v vz)
  prop "4ii"  $ (wcv tcpa s sz v vz)   ==> (wcv taumod s sz v vz)
  prop "4iii" $ (wcv taumod s sz v vz) ==> (wcv tep s sz v vz)

  ---------------------
  -- Local convexity --
  ---------------------
  prop "5a" $ lc tcpa
  prop "5b" $ lc taumod
  prop "5c" $ lc tep

  -- Invalid, but satisfiable.
  prop "bad_6" $ lc tau

  -- Invalid, but satisfiable.
  prop "6" $ not $ lc tau

--------------------------------------------------------------------------------

checkProp p backend = reify spec >>= prove (check (induct backend) p)

checkSat p backend = reify spec >>= prove (check (sat backend) p)

induct :: SmtFormat a => Backend a -> Prover
induct = kInduction (def {maxK = 0, debug = True})

sat :: SmtFormat a => Backend a -> Prover
sat = onlySat (def {debug = True})

kinduct :: SmtFormat a => Backend a -> Prover
kinduct = kInduction (def {debug = True})

