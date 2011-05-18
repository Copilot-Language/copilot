-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Copilot.Core
  ( module Language.Copilot.Core.Type
  , Spec (..)
  , WithSpec (..)
  , CanonStream (..)
  , Expr (..)
  , Fun1 (..)
  , Fun2 (..)
  , Fun3 (..)
  ) where

import Language.Copilot.Core.HeteroMap (HeteroMap, Key)
import Language.Copilot.Core.Type

-- | A specification:
data Spec :: ((* -> *) -> *) -> * -> * where
  Spec
    :: HeteroMap map
    => map (CanonStream (Key map))
    -> Expr (Key map) a
    -> Spec map a

data WithSpec a = WithSpec
  ( forall b .
      (
        forall map . HeteroMap map => Spec map a -> b
      ) -> b
  )

-- | Canonized streams:
data CanonStream :: (* -> *) -> * -> * where
  CanonStream
    :: Streamable a
    -- the stream buffer
    => [a]
    -- the expression for evaluating the stream
    -> Expr ref a
    -- the resulting stream
    -> CanonStream ref a

deriving instance Show (CanonStream ref a)

-- | None-recursive expressions:
data Expr :: (* -> *) -> * -> * where
  Const
    :: Streamable a
    => a
    -> Expr ref a
  Drop
    :: (Streamable a, Show (ref a))
    => Int
    -> ref a
    -> Expr ref a
  Extern
    :: Streamable a
    => String
    -> Expr ref a
  Fun1
    :: (Streamable a, Streamable b)
    => Fun1 a b
    -> Expr ref a
    -> Expr ref b
  Fun2
    :: (Streamable a, Streamable b, Streamable c)
    => Fun2 a b c
    -> Expr ref a
    -> Expr ref b
    -> Expr ref c
  Fun3
    :: (Streamable a, Streamable b, Streamable c, Streamable d)
    => Fun3 a b c d
    -> Expr ref a
    -> Expr ref b
    -> Expr ref c
    -> Expr ref d

deriving instance Show (Expr f a)

data Fun1 :: * -> * -> * where
  -- Boolean
  Not     :: Fun1 Bool Bool
  -- Numeric
  Abs     :: Num a => Fun1 a a
  Signum  :: Num a => Fun1 a a
  -- Fractional
  Recip   :: Fractional a => Fun1 a a
  -- Floating
  Exp     :: Floating a => Fun1 a a
  Sqrt    :: Floating a => Fun1 a a
  Log     :: Floating a => Fun1 a a
  Sin     :: Floating a => Fun1 a a
  Tan     :: Floating a => Fun1 a a
  Cos     :: Floating a => Fun1 a a
  Asin    :: Floating a => Fun1 a a
  Atan    :: Floating a => Fun1 a a
  Acos    :: Floating a => Fun1 a a
  Sinh    :: Floating a => Fun1 a a
  Tanh    :: Floating a => Fun1 a a
  Cosh    :: Floating a => Fun1 a a
  Asinh   :: Floating a => Fun1 a a
  Atanh   :: Floating a => Fun1 a a
  Acosh   :: Floating a => Fun1 a a

deriving instance Eq (Fun1 a b)

deriving instance Show (Fun1 a b)

data Fun2 :: * -> * -> * -> * where
  -- Boolean
  And     :: Fun2 Bool Bool Bool
  Or      :: Fun2 Bool Bool Bool
  -- Numeric
  Add     :: Num a => Fun2 a a a
  Sub     :: Num a => Fun2 a a a
  Mul     :: Num a => Fun2 a a a
  -- Fractional
  Div     :: Fractional a => Fun2 a a a
  -- Integral
  Mod     :: Integral a => Fun2 a a a
  -- Equality
  Eq      :: Eq a  => Fun2 a a Bool
  Ne      :: Eq a  => Fun2 a a Bool
  -- Relational
  Lt      :: Ord a => Fun2 a a Bool
  Gt      :: Ord a => Fun2 a a Bool
  Le      :: Ord a => Fun2 a a Bool
  Ge      :: Ord a => Fun2 a a Bool
  -- Floating
  Pow     :: Floating a => Fun2 a a a
  LogBase :: Floating a => Fun2 a a a

deriving instance Eq (Fun2 a b c)

deriving instance Show (Fun2 a b c)

data Fun3 :: * -> * -> * -> * -> * where
  -- Mutex (a.k.a. if-then-else)
  Mux :: Fun3 Bool a a a

deriving instance Eq (Fun3 a b c d)

deriving instance Show (Fun3 a b c d)
