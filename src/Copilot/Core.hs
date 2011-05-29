-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.

-- | Intermediate representation for Copilot specifications.
-- The form of the representation is based on this paper:
--
-- * Carette, Jacques and Kiselyov, Oleg and Shan, Chung-chieh,
-- \"/Finally tagless, partially evaluated: Tagless staged/
-- /interpreters for simpler typed languages/\",
-- Journal of Functional Programming vol. 19, p. 509-543, 2009.
--
-- The following article might also be useful:
--
-- * Guillemette, Louis-Julien and Monnier, Stefan,
-- \"/Type-Safe Code Transformations in Haskell/\",
-- Electronic Notes in Theoretical Computer Science vol. 174, p. 23-39, 2007.
--
-- For examples of how to traverse a Copilot specification see
-- the source code of the interpreter
-- ("Copilot.Core.Interpret")
-- and the pretty-printer
-- ("Copilot.Core.PrettyPrint").

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Copilot.Core
  ( module Copilot.Core.Type
  , Expr (..)
  , Op1 (..)
  , Op2 (..)
  , Op3 (..)
  , Name
  , Id
  , Stream (..)
  , Trigger (..)
  , Spec (..)
  )
  where

import Copilot.Core.Type
--import qualified Copilot.Core.HeteroMap as H

-- | The expression class.
class Expr η where
  -- | A constant.
  const
    :: Streamable α
    => α
    -> η α
  -- | The temporal look-ahead operator.
  drop
    :: Streamable α
    => Int
    -> Id
    -> η α
  -- | An external variable.
  extern
    :: Streamable α
    => Name
    -> η α
  -- | An unary operator.
  op1
    :: (Streamable α, Streamable β)
    => (forall θ . Op1 θ => θ α β)
    -> η α -> η β
  -- | A binary operator.
  op2
    :: (Streamable α, Streamable β, Streamable γ)
    => (forall θ . Op2 θ => θ α β γ)
    -> η α -> η β -> η γ
  -- | A Ternary operator.
  op3
    :: (Streamable α, Streamable β, Streamable γ, Streamable δ)
    => (forall θ . Op3 θ => θ α β γ δ)
    -> η α -> η β -> η γ -> η δ

-- | Unary operators.
class Op1 θ where
  -- Boolean operators.
  not  :: θ Bool Bool
  -- Numeric operators.
  abs  :: Num α => θ α α
  sign :: Num α => θ α α

-- | Binary operators.
class Op2 θ where
  -- Boolean operators.
  and  :: θ Bool Bool Bool
  or   :: θ Bool Bool Bool
  -- Numeric operators.
  add  :: Num α => θ α α α
  sub  :: Num α => θ α α α
  mul  :: Num α => θ α α α
  -- Integral operators.
  mod  :: Integral α => θ α α α 
  div  :: Integral α => θ α α α 
  -- Equality operators.
  eq   :: Eq α => θ α α Bool
  ne   :: Eq α => θ α α Bool
  -- Relational operators.
  le   :: Ord α => θ α α Bool
  ge   :: Ord α => θ α α Bool
  lt   :: Ord α => θ α α Bool
  gt   :: Ord α => θ α α Bool

-- | Ternary operators.
class Op3 θ where
  mux  :: θ Bool α α α

-- | A name of a trigeer or an external variable or function.
type Name = String

-- | A stream identifier.
type Id = Int

-- | A stream.
data Stream = forall α . Streamable α => Stream
  { streamId     :: Id
  , streamBuffer :: [α]
  , streamGuard  :: forall η . Expr η => Maybe (η Bool)
  , streamValue  :: forall η . Expr η => η α
  }

-- | A trigger.
data Trigger = forall α . Streamable α => Trigger
  { triggerName  :: Name
  , triggerGuard :: forall η . Expr η => η Bool
  , triggerValue :: forall η . Expr η => η α
  }

-- | A Copilot specification consists of a
-- list of anomymous streams and a list of triggers.
data Spec = Spec
  { specStreams  :: [Stream]
  , specTriggers :: [Trigger]
  }
