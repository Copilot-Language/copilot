--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
{-# LANGUAGE ExistentialQuantification, GADTs #-}

module Copilot.Core.Type.Eq
  ( EqWit (..)
  , eqWit
  , UVal (..)
  ) where

import Copilot.Core.Error (impossible)
import Copilot.Core.Type
import Copilot.Core.Type.Dynamic (fromDyn, toDyn)

--------------------------------------------------------------------------------

data EqWit a = Eq a => EqWit

--------------------------------------------------------------------------------

eqWit :: Type a -> EqWit a
eqWit t =
  case t of
    Bool   -> EqWit
    Int8   -> EqWit
    Int16  -> EqWit
    Int32  -> EqWit
    Int64  -> EqWit
    Word8  -> EqWit
    Word16 -> EqWit
    Word32 -> EqWit
    Word64 -> EqWit
    Float  -> EqWit
    Double -> EqWit
    Struct -> EqWit

--------------------------------------------------------------------------------

data UVal = forall a. UVal
  { uType :: Type a
  , uVal  :: a
  }

--------------------------------------------------------------------------------

instance Eq UVal where
  (==) UVal { uType = t1
            , uVal = a } 
       UVal { uType = t2
            , uVal = b }
    = case eqWit t1 of
        EqWit -> 
          case eqWit t2 of
            EqWit -> 
              let dyn1 = toDyn t1 a in
              case fromDyn t2 dyn1 of
                Nothing -> False
                Just x  -> 
                  case t1 of 
                    -- Hacks for QuickChecking between C and Haskell
                    Float  -> case t2 of
                                Float -> approx floatErr x b
                                _     -> impossible "instance Eq UVal"
                                                    "copilot-core"
                    Double -> case t2 of
                                Double -> approx doubleErr x b
                                _      -> impossible "instance Eq UVal" 
                                                     "copilot-core"
                    _      -> x == b
    where 
    approx :: (Num a, Ord a) => a -> a -> a -> Bool
    approx err x y = x - y < err && y - x < err
    floatErr  = 0.0001 :: Float
    doubleErr = 0.0001 :: Double

--------------------------------------------------------------------------------
