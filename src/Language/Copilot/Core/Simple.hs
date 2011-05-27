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
-- ("Language.Copilot.Core.Interpret")
-- and the pretty-printer
-- ("Language.Copilot.Core.PrettyPrint").

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Language.Copilot.Core
  ( module Language.Copilot.Core.Type
  , Expr (..)
  , Op1 (..)
  , Op2 (..)
  , Op3 (..)
  , Name
  , Buff
  , Strm (..)
  , Trig (..)
  , Spec (..)
  , Expr__
  )
  where

import Language.Copilot.Core.Type
import qualified Language.Copilot.Core.HeteroMap as H

-- | The expression class.
class Expr η where
  -- | The temporal look-ahead operator.
  drp
    :: Typed α
    => Int
    -> H.Key
    -> η α
  -- | A literal.
  lit
    :: (Typed α, Show α)
    => α
    -> η α
  -- | An external variable.
  ext
    :: Typed α
    => Name
    -> η α
  -- | An unary operator.
  op1
    :: (Typed α, Typed β)
    => (forall θ . Op1 θ => θ α β)
    -> η α -> η β
  -- | A binary operator.
  op2
    :: (Typed α, Typed β, Typed γ)
    => (forall θ . Op2 θ => θ α β γ)
    -> η α -> η β -> η γ
  -- | A Ternary operator.
  op3
    :: (Typed α, Typed β, Typed γ, Typed δ)
    => (forall θ . Op3 θ => θ α β γ δ)
    -> η α -> η β -> η γ -> η δ

-- | The unary operator class.
class Op1 θ where
  -- Boolean operators.
  not'    :: θ Bool Bool
  -- Numeric operators.
  abs'    :: Num α => θ α α
  signum' :: Num α => θ α α

-- | The binary operator class.
class Op2 θ where
  -- Boolean operators.
  (&&.) :: θ Bool Bool Bool
  (||.) :: θ Bool Bool Bool
  -- Numeric operators.
  (+.)  :: Num α => θ α α α
  (-.)  :: Num α => θ α α α
  (*.)  :: Num α => θ α α α
  -- Equality operators.
  (==.) :: Eq α => θ α α Bool
  (/=.) :: Eq α => θ α α Bool
  -- Relational operators.
  (<=.) :: Ord α => θ α α Bool
  (>=.) :: Ord α => θ α α Bool
  (<.)  :: Ord α => θ α α Bool
  (>.)  :: Ord α => θ α α Bool

-- | The ternary operator class.
class Op3 θ where
  if_then_else :: θ Bool α α α

-- | A name of an external variable or function.
type Name = String

-- | A buffer.
type Buff α = [α]

-- | A stream.
data Strm α = Show α =>
  Strm (Buff α) (Expr__ α)

-- | A trigger.
data Trig = forall α . Typed α =>
  Trig Name (Expr__ Bool) (Expr__ α)

-- | A Copilot specification consists of a heterogenous map of streams
-- and a list of triggers.
data Spec = forall φ . H.Map φ => Spec (φ Strm) [Trig]

-- A convenient shorthand for universally quantified expressions.
type Expr__ α = forall η . Expr η => η α
