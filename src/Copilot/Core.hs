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
  ( Expr (..)
  , Op1 (..)
  , Op2 (..)
  , Op3 (..)
  , Op4 (..)
  , Name
  , Id
  , Stream (..)
  , Trigger (..)
  , Spec (..)
  )
  where

import Copilot.Core.Saturable
import Copilot.Core.Type
import Copilot.Core.Uninitializable
import Data.Int
import Data.Word

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
  -- | Quaternary operator.
  op4
    :: (Streamable α, Streamable β, Streamable γ, Streamable δ, Streamable ξ)
    => (forall θ . Op4 θ => θ α β γ δ ξ)
    -> η α -> η β -> η γ -> η δ -> η ξ

-- | Unary operators.
class Op1 θ where
  -- Boolean operators.
  not  :: θ Bool Bool
  -- Numeric operators.
  abs  :: Num α => θ α α
  sign :: Num α => θ α α
  -- Tuple operators:
  untup2_1 :: θ (α, β) α
  untup2_2 :: θ (α, β) β
  untup3_1 :: θ (α, β, γ) α
  untup3_2 :: θ (α, β, γ) β
  untup3_3 :: θ (α, β, γ) γ
  untup4_1 :: θ (α, β, γ, δ) α
  untup4_2 :: θ (α, β, γ, δ) β
  untup4_3 :: θ (α, β, γ, δ) γ
  untup4_4 :: θ (α, β, γ, δ) δ

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
  -- Tuple operator:
  tup2 :: θ α β (α, β)

-- | Ternary operators.
class Op3 θ where
  -- Conditional operator:
  mux  :: θ Bool α α α
  -- Tuple operator:
  tup3 :: θ α β γ (α, β, γ)

-- | Quaternary operators.
class Op4 θ where
  -- Tuple operator:
  tup4 :: θ α β γ δ (α, β, γ, δ)

-- | A name of a trigger, an external variable, or an external function.
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

class
  ( Saturable α
  , Show α
  , Typed α
  , Uninitializable α
  ) => Streamable α

instance Streamable Bool
instance Streamable Int8
instance Streamable Int16
instance Streamable Int32
instance Streamable Int64
instance Streamable Word8
instance Streamable Word16
instance Streamable Word32
instance Streamable Word64

instance (Streamable α, Streamable β)
  => Streamable (α, β)

instance (Streamable α, Streamable β, Streamable γ)
  => Streamable (α, β, γ)

instance (Streamable α, Streamable β, Streamable γ, Streamable δ)
  => Streamable (α, β, γ, δ)
