-- |

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Copilot.Node
  ( Mu2 (..)
  , Node (..)
  , Fun1 (..)
  , Fun2 (..)
  , Fun3 (..)
  , fmap2
  , foldMap2
  , foldr2
  , traverse2
  ) where

import Control.Applicative (Applicative (..), (<$>))
import Data.Monoid (Monoid (..), Endo (..))
import Language.Copilot.Streamable (Streamable)
import Language.Copilot.Array (Array)

data Mu2 ff a = In (ff (Mu2 ff) a)

data Node :: (* -> *) -> * -> * where
  Const
    :: Streamable a
    => a
    -> Node f a
  Append :: Streamable a
    => [a]
    -> f a
    -> Node f a
  Drop
    :: Streamable a
    => Int
    -> f a
    -> Node f a
  Extern
    :: Streamable a
    => String
    -> Node f a
  Fun1
    :: (Streamable a, Streamable b)
    => Fun1 a b
    -> f a
    -> Node f b
  Fun2
    :: (Streamable a, Streamable b, Streamable c)
    => Fun2 a b c
    -> f a
    -> f b
    -> Node f c
  Fun3
    :: (Streamable a, Streamable b, Streamable c, Streamable d)
    => Fun3 a b c d
    -> f a
    -> f b
    -> f c
    -> Node f d

data Fun1 :: * -> * -> * where
  -- Boolean
  Not :: Fun1 Bool Bool
  -- Numeric
  Abs :: (Streamable a, Num a) => Fun1 a a
  Sgn :: (Streamable a, Num a) => Fun1 a a
  -- Fractional
  Rcp :: (Streamable a, Fractional a) => Fun1 a a

deriving instance Eq (Fun1 a b)

deriving instance Show (Fun1 a b)

data Fun2 :: * -> * -> * -> * where
  -- Boolean
  And :: Fun2 Bool Bool Bool
  Or  :: Fun2 Bool Bool Bool
  -- Numeric
  Add :: (Streamable a, Num a) => Fun2 a a a
  Sub :: (Streamable a, Num a) => Fun2 a a a
  Mul :: (Streamable a, Num a) => Fun2 a a a
  -- Fractional
  Div :: (Streamable a, Fractional a) => Fun2 a a a
  -- Integral
  Mod :: (Streamable a, Integral a) => Fun2 a a a
  -- Equality
  Eq  :: (Streamable a, Eq a)  => Fun2 a a Bool
  Ne  :: (Streamable a, Eq a)  => Fun2 a a Bool
  -- Relational
  Lt  :: (Streamable a, Ord a) => Fun2 a a Bool
  Gt  :: (Streamable a, Ord a) => Fun2 a a Bool
  Le  :: (Streamable a, Ord a) => Fun2 a a Bool
  Ge  :: (Streamable a, Ord a) => Fun2 a a Bool
  -- Array
  Idx :: (Streamable a, Streamable i, Integral i) => Fun2 (Array a) i a

deriving instance Eq (Fun2 a b c)

deriving instance Show (Fun2 a b c)

data Fun3 :: * -> * -> * -> * -> * where
  -- Mutex (a.k.a. if-then-else)
  Mux :: Streamable a => Fun3 Bool a a a

deriving instance Eq (Fun3 a b c d)

deriving instance Show (Fun3 a b c d)

fmap2
  :: (forall b . Streamable b => f b -> g b)
  -> Node f a
  -> Node g a
fmap2 f n0 = case n0 of
  Const x         -> Const x
  Append xs t     -> Append xs (f t)
  Drop k t        -> Drop k (f t)
  Extern cs       -> Extern cs
  Fun1 g t        -> Fun1 g (f t)
  Fun2 g t1 t2    -> Fun2 g (f t1) (f t2)
  Fun3 g t1 t2 t3 -> Fun3 g (f t1) (f t2) (f t3)

foldMap2
  :: Monoid m
  => (forall b . f b -> m)
  -> Node f a
  -> m
foldMap2 f n0 = case n0 of
  Const _         -> mempty
  Append _  t     -> f t
  Drop _ t        -> f t
  Extern _        -> mempty
  Fun1 _ t        -> f t
  Fun2 _ t1 t2    -> f t1 `mappend` f t2
  Fun3 _ t1 t2 t3 -> f t1 `mappend` f t2 `mappend` f t3

foldr2
  :: (forall a . f a -> b -> b)
  -> b
  -> Node f c
  -> b
foldr2 f acc n0 = appEndo (foldMap2 (Endo . f) n0) acc

traverse2
  :: Applicative m
  => (forall b . Streamable b => f b -> m (g b))
  -> Node f a
  -> m (Node g a)
traverse2 f n0 = case n0 of
  Const x         -> pure $ Const x
  Append xs t     -> Append xs <$> f t
  Drop k t        -> Drop k <$> f t
  Extern cs       -> pure $ Extern cs
  Fun1 g t        -> Fun1 g <$> f t
  Fun2 g t1 t2    -> Fun2 g <$> f t1 <*> f t2
  Fun3 g t1 t2 t3 -> Fun3 g <$> f t1 <*> f t2 <*> f t3
