module WCV where

import Prelude ()

import Copilot.Language
import Copilot.Language.Reify
import Copilot.Theorem
import Copilot.Theorem.Prover.Z3

import qualified Copilot.Language.Operators.Propositional as P

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
tau s v = mux (s |*| v < 0) ((-(sq s)) / (s |*| v)) (-1)

tcpa :: Vect2 -> Vect2 -> Stream Double
tcpa s v@(vx, vy) = mux (vx ~= 0 && vy ~= 0) 0 (-(s |*| v)/(sq v))

taumod :: Vect2 -> Vect2 -> Stream Double
taumod s v = mux (s |*| v < 0) ((dthr * dthr - (sq s))/(s |*| v)) (-1)

tep :: Vect2 -> Vect2 -> Stream Double
tep s v = mux ((s |*| v < 0) && ((delta s v dthr) >= 0))
  (theta s v dthr (-1))
  (-1)

delta :: Vect2 -> Vect2 -> Stream Double -> Stream Double
delta s v d = (d*d) * (sq v) - ((det s v)*(det s v))
-- Here the formula says : (s . orth v)^2 which is the same as det(s,v)^2

theta :: Vect2 -> Vect2 -> Stream Double -> Stream Double -> Stream Double
theta s v d e = (-(s |*| v) + e * (sqrt $ delta s v d)) / (sq v)

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

--------------
-- Theorems --
--------------

-------------------------
-- Horizontal symmetry --
-------------------------
horizSymmetry = do
  theorem "1a" (forall $ (tau s v)    ~= (tau (neg s) (neg v)))     arith
  theorem "1b" (forall $ (tcpa s v)   ~= (tcpa (neg s) (neg v)))    arith
  theorem "1c" (forall $ (taumod s v) ~= (taumod (neg s) (neg v)))  arith
  theorem "1d" (forall $ (tep s v)    ~= (tep (neg s) (neg v)))     arith

-------------------------
-- Horizontal ordering --
-------------------------
horizOrdering = do
  theorem "2a" (forall $ ((s |*| v) < 0 && (norm s) > dthr && (dcpa s v) <= dthr)
    ==> ((tep s v) <= (taumod s v)))
    arith
  theorem "2b" (forall $ ((s |*| v) < 0 && (norm s) > dthr && (dcpa s v) <= dthr)
    ==> ((taumod s v) <= (tcpa s v)))
    arith
  theorem "2c" (forall $ ((s |*| v) < 0 && (norm s) > dthr && (dcpa s v) <= dthr)
    ==> ((tcpa s v) <= (tau s v)))
    arith

--------------
-- Symmetry --
--------------
symmetry = do
  theorem "3a" (forall $ (wcv tau s sz v vz)    == (wcv tau (neg s) (-sz) (neg v) (-vz)))
    arith
  theorem "3b" (forall $ (wcv tcpa s sz v vz)   == (wcv tcpa (neg s) (-sz) (neg v) (-vz)))
    arith
  theorem "3c" (forall $ (wcv taumod s sz v vz) == (wcv taumod (neg s) (-sz) (neg v) (-vz)))
    arith
  theorem "3d" (forall $ (wcv tep s sz v vz)    == (wcv tep (neg s) (-sz) (neg v) (-vz)))
    arith

---------------
-- Inclusion --
---------------
inclusion = do
  theorem "4i"   (forall $ (wcv tau s sz v vz)    ==> (wcv tcpa s sz v vz))
    arith
  theorem "4ii"  (forall $ (wcv tcpa s sz v vz)   ==> (wcv taumod s sz v vz ))
    arith
  theorem "4iii" (forall $ (wcv taumod s sz v vz) ==> (wcv tep s sz v vz))
    arith

---------------------
-- Local convexity --
---------------------

t1, t2, t3 :: Stream Double
t1 = extern "t1" Nothing
t2 = extern "t2" Nothing
t3 = extern "t3" Nothing

locallyConvex :: (Vect2 -> Vect2 -> Stream Double) -> Stream Bool
locallyConvex tvar = (0 <= t1 && t1 <= t2 && t2 <= t3)
   ==> not ( (wcv tvar (sx + t1*vx, sy + t1*vy) (sz + t1*vz) v vz)
      &&  (not $ wcv tvar (sx + t2*vx, sy + t2*vy) (sz + t2*vz) v vz)
      &&  (wcv tvar (sx + t3*vx, sy + t3*vy) (sz + t3*vz) v vz))

localConvexity = do
  theorem "5a" (forall $ locallyConvex tcpa)        arith
  theorem "5b" (forall $ locallyConvex taumod)      arith
  theorem "5c" (forall $ locallyConvex tep)         arith
  theorem "6"  (P.not (forall $ locallyConvex tau)) arithSat

--------------------------------------------------------------------------------

arith :: Proof Universal
arith    = onlyValidity def { nraNLSat = True, debug = False }

arithSat :: Proof Existential
arithSat = onlySat      def { nraNLSat = True, debug = False }
