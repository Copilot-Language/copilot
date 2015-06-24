---------------------------------------------------------------

module Copilot.Kind.Cnub.Spec
  ( module Copilot.Kind.Misc.Operators
  , module Copilot.Kind.Misc.Type

  , Expr      (..)
  , Stream    (..)
  , UnintFun  (..)
  , Cnub      (..)

  , StreamId
  , UnintId
  , PropId
  , ObserverId
  ) where

import Copilot.Kind.Misc.Type
import Copilot.Kind.Misc.Operators
import Copilot.Kind.Misc.Utils (Map)

--------------------------------------------------------------------------------

type StreamId    = String
type UnintId     = String
type PropId      = String
type ObserverId  = String

--------------------------------------------------------------------------------

data Expr a where
  Const :: Type a -> a -> Expr a
  Op1   :: Type b -> Op1 a b -> Expr a -> Expr b
  Op2   :: Type c -> Op2 a b c -> Expr a -> Expr b -> Expr c
  Ite   :: Type t -> Expr Bool -> Expr t -> Expr t -> Expr t
  Drop  :: Type a -> Int -> StreamId -> Expr a

  Unint :: Type a -> UnintId -> [U Expr] -> Expr a

--------------------------------------------------------------------------------

data Stream = forall a . Stream
  { streamBuffer :: [a]
  , streamType   :: Type a
  , streamExpr   :: Expr a }

data UnintFun = forall a . UnintFun
  { unintType :: Type a
  , unintArgs :: [U Type] }

data Cnub = Cnub
  { specStreams    :: Map StreamId Stream
  , specProperties :: Map PropId StreamId
  , specObservers  :: Map ObserverId StreamId
  , specUnints     :: Map UnintId UnintFun }

--------------------------------------------------------------------------------
