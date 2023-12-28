{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | A version of WCV with input in spherical coordinates.
module SphericalWCV where

import Prelude ()

import Copilot.Language
import Copilot.Language.Reify
import Copilot.Theorem
import Copilot.Theorem.Prover.Z3

import qualified Copilot.Language.Operators.Propositional as P

type Vect2 = (Stream Double, Stream Double)
type Vect3 = (Stream Double, Stream Double, Stream Double)

-- | Ownship: lat, lon (rad); ground speed, track, vert speed (m/s); altitude (m).
latO, lonO, gsO, trkO, vsO, altO :: Stream Double
latO = extern "latO" Nothing
lonO = extern "lonO" Nothing
gsO  = extern "gsO" Nothing
trkO = extern "trkO" Nothing
vsO  = extern "vsO" Nothing
altO = extern "altO" Nothing

-- | Intruder: lat, lon (rad); ground speed, track, vert speed (m/s); altitude (m).
latI, lonI, gsI, trkI, vsI, altI :: Stream Double
latI = extern "latI" Nothing
lonI = extern "lonI" Nothing
gsI  = extern "gsI" Nothing
trkI = extern "trkI" Nothing
vsI  = extern "vsI" Nothing
altI = extern "altI" Nothing

dthr, tthr, zthr, tcoathr :: Stream Double
dthr    = extern "dthr" Nothing
tthr    = extern "tthr" Nothing
zthr    = extern "zthr" Nothing
tcoathr = extern "tcoathr" Nothing

spherical2xyz :: Stream Double -> Stream Double -> Vect3
spherical2xyz lat lon = (x, y, z)
      where
        r     = 6371000 -- Radius of the earth in meters
        theta = pi / 2 - lat
        phi   = pi - lon
        x     = r * sin theta * cos phi
        y     = r * sin theta * sin phi
        z     = r * cos theta

dot3 :: Vect3 -> Vect3 -> Stream Double
dot3 (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

norm3 :: Vect3 -> Stream Double
norm3 v = sqrt (v `dot3` v)

vect3_orthog_toy :: Vect3 -> Vect3
vect3_orthog_toy (x, y, _) = (mux (x /= 0 || y /= 0) y 1, mux (x /= 0 || y /= 0) (-x) 0, constD 0)

cross3 :: Vect3 -> Vect3 -> Vect3
cross3 (x1, y1, z1) (x2, y2, z2) = (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)

vect3_orthog_toz :: Vect3 -> Vect3
vect3_orthog_toz v = v `cross3` vect3_orthog_toy v

unit :: Vect3 -> Vect3
unit (x, y, z) = (x / n, y / n, z / n)
      where
        n = norm3 (x, y, z)

vect3_orthonorm_toy :: Vect3 -> Vect3
vect3_orthonorm_toy = unit . vect3_orthog_toy

vect3_orthonorm_toz :: Vect3 -> Vect3
vect3_orthonorm_toz = unit . vect3_orthog_toz

sphere_to_2D_plane :: Vect3 -> Vect3 -> Vect2
sphere_to_2D_plane nzv w = (ymult `dot3` w, zmult `dot3` w)
      where
        ymult = vect3_orthonorm_toy nzv
        zmult = vect3_orthonorm_toz nzv

pO :: Vect3
pO = spherical2xyz latO lonO

pI :: Vect3
pI = spherical2xyz latI lonI

sOx, sOy, sOz :: Stream Double
(sOx, sOy, sOz) = (0, 0, altO)

sIx, sIy, sIz :: Stream Double
(sIx, sIy, sIz) = (sI2x, sI2y, altI)
      where
        (sI2x, sI2y) = sphere_to_2D_plane pO pI

vOx, vOy, vOz :: Stream Double
(vOx, vOy, vOz) = (gsO * sin trkO, gsO * cos trkO, vsO)

vIx, vIy, vIz :: Stream Double
(vIx, vIy, vIz) = (gsI * sin trkI, gsI * cos trkI, vsI)

-- latI velocity/position --

vx, vy, vz :: Stream Double
(vx, vy, vz) = (vOx - vIx, vOy - vIy, vOz - vIz)

v :: Vect2
v = (vx, vy)

sx, sy, sz :: Stream Double
(sx, sy, sz) = (sOx - sIx, sOy - sIy, sOz - sIz)

s :: Vect2
s = (sx, sy)

-- Vector stuff --

(|*|) :: Vect2 -> Vect2 -> Stream Double
(|*|) (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

sq :: Vect2 -> Stream Double
sq x = x |*| x

norm :: Vect2 -> Stream Double
norm = sqrt . sq

det :: Vect2 -> Vect2 -> Stream Double
det (x1, y1) (x2, y2) = (x1 * y2) - (x2 * y1)

(~=) :: Stream Double -> Stream Double -> Stream Bool
a ~= b = abs (a - b) < 0.001

neg :: Vect2 -> Vect2
neg (x, y) = (negate x, negate y)

-- Time variables --

tau :: Vect2 -> Vect2 -> Stream Double
tau s v = mux (s |*| v < 0) ((-(sq s)) / (s |*| v)) (-1)

tcpa :: Vect2 -> Vect2 -> Stream Double
tcpa s v@(vx, vy) = mux (vx ~= 0 && vy ~= 0) 0 (-(s |*| v) / sq v)

taumod :: Vect2 -> Vect2 -> Stream Double
taumod s v = mux (s |*| v < 0) ((dthr * dthr - sq s)/(s |*| v)) (-1)

tep :: Vect2 -> Vect2 -> Stream Double
tep s v = mux ((s |*| v < 0) && (delta s v dthr >= 0))
  (theta s v dthr (-1))
  (-1)

delta :: Vect2 -> Vect2 -> Stream Double -> Stream Double
delta s v d = d * d * sq v - (det s v * det s v)
-- Here the formula says : (s . orth v)^2 which is the same as det(s,v)^2

theta :: Vect2 -> Vect2 -> Stream Double -> Stream Double -> Stream Double
theta s v d e = (-(s |*| v) + e * sqrt (delta s v d)) / sq v

-- Some tools for times --

tcoa :: Stream Double -> Stream Double -> Stream Double
tcoa sz vz = mux ((sz * vz) < 0) ((-sz) / vz) (-1)

dcpa :: Vect2 -> Vect2 -> Stream Double
dcpa s@(sx, sy) v@(vx, vy) = norm (sx + tcpa s v * vx, sy + tcpa s v * vy)

-- Well clear Violation --

wcv :: (Vect2 -> Vect2 -> Stream Double) ->
       Vect2 -> Stream Double ->
       Vect2 -> Stream Double ->
       Stream Bool
wcv tvar s sz v vz = horizontalWCV tvar s v && verticalWCV sz vz

verticalWCV :: Stream Double -> Stream Double -> Stream Bool
verticalWCV sz vz = (abs sz <= zthr) || (0 <= tcoa sz vz && tcoa sz vz <= tcoathr)

horizontalWCV :: (Vect2 -> Vect2 -> Stream Double) -> Vect2 -> Vect2 -> Stream Bool
horizontalWCV tvar s v = (norm s <= dthr) || ((dcpa s v <= dthr) && (0 <= tvar s v) && (tvar s v <= tthr))

-- Theorems --

-- Horizontal symmetry --
horizSymmetry = do
  theorem "1a" (forAll $ tau s v    ~= tau (neg s) (neg v))     arith
  theorem "1b" (forAll $ tcpa s v   ~= tcpa (neg s) (neg v))    arith
  theorem "1c" (forAll $ taumod s v ~= taumod (neg s) (neg v))  arith
  theorem "1d" (forAll $ tep s v    ~= tep (neg s) (neg v))     arith

-- Horizontal ordering --
horizOrdering = do
  theorem "2a" (forAll $ ((s |*| v) < 0 && norm s > dthr && dcpa s v <= dthr)
    ==> (tep s v <= taumod s v))
    arith
  theorem "2b" (forAll $ ((s |*| v) < 0 && norm s > dthr && dcpa s v <= dthr)
    ==> (taumod s v <= tcpa s v))
    arith
  theorem "2c" (forAll $ ((s |*| v) < 0 && norm s > dthr && dcpa s v <= dthr)
    ==> (tcpa s v <= tau s v))
    arith

-- Symmetry --
symmetry = do
  theorem "3a" (forAll $ wcv tau s sz v vz    == wcv tau (neg s) (-sz) (neg v) (-vz))
    arith
  theorem "3b" (forAll $ wcv tcpa s sz v vz   == wcv tcpa (neg s) (-sz) (neg v) (-vz))
    arith
  theorem "3c" (forAll $ wcv taumod s sz v vz == wcv taumod (neg s) (-sz) (neg v) (-vz))
    arith
  theorem "3d" (forAll $ wcv tep s sz v vz    == wcv tep (neg s) (-sz) (neg v) (-vz))
    arith

-- Inclusion --
inclusion = do
  theorem "4i"   (forAll $ wcv tau s sz v vz    ==> wcv tcpa s sz v vz)
    arith
  theorem "4ii"  (forAll $ wcv tcpa s sz v vz   ==> wcv taumod s sz v vz )
    arith
  theorem "4iii" (forAll $ wcv taumod s sz v vz ==> wcv tep s sz v vz)
    arith

-- Local convexity --

t1, t2, t3 :: Stream Double
t1 = extern "t1" Nothing
t2 = extern "t2" Nothing
t3 = extern "t3" Nothing

locallyConvex :: (Vect2 -> Vect2 -> Stream Double) -> Stream Bool
locallyConvex tvar = (0 <= t1 && t1 <= t2 && t2 <= t3)
   ==> not ( wcv tvar (sx + t1*vx, sy + t1*vy) (sz + t1*vz) v vz
      &&  not (wcv tvar (sx + t2*vx, sy + t2*vy) (sz + t2*vz) v vz)
      &&  wcv tvar (sx + t3*vx, sy + t3*vy) (sz + t3*vz) v vz)

localConvexity = do
  theorem "5a" (forAll $ locallyConvex tcpa)        arith
  theorem "5b" (forAll $ locallyConvex taumod)      arith
  theorem "5c" (forAll $ locallyConvex tep)         arith
  theorem "6"  (P.not (forAll $ locallyConvex tau)) arithSat

arith :: Proof Universal
arith    = onlyValidity def { nraNLSat = True, debug = False }

arithSat :: Proof Existential
arithSat = onlySat      def { nraNLSat = True, debug = False }
