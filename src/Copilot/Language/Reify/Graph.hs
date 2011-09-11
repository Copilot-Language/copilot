--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

-- | Transforms a Copilot stream into a graph.

module Copilot.Language.Reify.Graph
  ( G (..)
  ) where

import Control.Applicative (pure, (<$>), (<*>))
import Copilot.Core (Typed)
import qualified Copilot.Core as Core
import Copilot.Language.Stream
import Data.Reify (MuRef (..))

data G t where
  AppendG
    :: Typed a
    => [a]
    -> t
    -> G t
  ConstG
    :: Typed a
    => a
    -> G t
  DropG
    :: Int
    -> t
    -> G t
  Op1G
    :: (Typed a, Typed b)
    => Core.Op1 a b
    -> t
    -> G t
  Op2G
    :: (Typed a, Typed b, Typed c)
    => Core.Op2 a b c
    -> t -> t
    -> G t
  Op3G
    :: (Typed a, Typed b, Typed c, Typed d)
    => Core.Op3 a b c d
    -> t -> t -> t
    -> G t

instance MuRef (Stream a) where
  type DeRef (Stream a) = G

  mapDeRef f e0 = case e0 of
    Append xs _ e   -> AppendG xs <$> f e
    Const x         -> pure (ConstG x)
    Drop k e        -> DropG k <$> f e
    Op1 op e        -> Op1G op <$> f e
    Op2 op e1 e2    -> Op2G op <$> f e1 <*> f e2
    Op3 op e1 e2 e3 -> Op3G op <$> f e1 <*> f e2 <*> f e3
    _               -> error "Not implemented in Graph.hs in copilot-language!"

--type Env = Map String (Stream a)

--mapDeRef' :: Env -> 

{-
instance Functor G where
  fmap f g = case g of
    AppendG xs t     -> AppendG xs (f t)
    ConstG x         -> ConstG x
    DropG k t        -> DropG k (f t)
    Op1G op t        -> Op1G op (f t)
    Op2G op t1 t2    -> Op2G op (f t1) (f t2)
    Op3G op t1 t2 t3 -> Op3G op (f t1) (f t2) (f t3)

instance Foldable G where
  foldMap f g = case g of
    AppendG _ t      -> f t
    ConstG _         -> mempty
    DropG _ t        -> f t
    Op1G _ t         -> f t
    Op2G _ t1 t2     -> f t1 `mappend` f t2
    Op3G _ t1 t2 t3  -> f t1 `mappend` f t2

instance Traversable G where
  traverse f g = case g of
    AppendG xs t     -> AppendG xs <$> f t
    ConstG x         -> pure (ConstG x)
    DropG k t        -> DropG k <$> f t
    Op1G op t        -> Op1G op <$> f t
    Op2G op t1 t2    -> Op2G op <$> f t1 <*> f t2
    Op3G op t1 t2 t3 -> Op3G op <$> f t1 <*> f t2 <*> f t3
-}

{-
data Mu t = In (t (Mu t))

s2g :: Stream a -> Mu G
s2g (Append xs _ e)   = In (AppendG xs (s2g e))
s2g (Const x)         = In (ConstG x)
s2g (Drop k e)        = In (DropG k (s2g e))
s2g (Op1 op e)        = In (Op1G op (s2g e))
s2g (Op2 op e1 e2)    = In (Op2G op (s2g e1) (s2g e2))
s2g (Op3 op e1 e2 e3) = In (Op3G op (s2g e1) (s2g e2) (s2g e3))
-}
