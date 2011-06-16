--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Compile.C99.MetaTable
  ( StreamInfo (..)
  , ExternInfo (..)
  , LetInfo (..)
  , StreamInfoMap
  , ExternInfoMap
  , LetInfoMap
  , MetaTable (..)
  , allocMetaTable
  ) where

import Control.Monad (liftM)
import qualified Copilot.Compile.C99.Queue as Q
import qualified Copilot.Compile.C99.Witness as W
import qualified Copilot.Core as C
import Copilot.Core.Spec.Externals (Extern (..), externals)
import Data.Map (Map)
import qualified Data.Map as M
import Language.Atom (Atom)
import qualified Language.Atom as A
import Prelude hiding (id)

--------------------------------------------------------------------------------

data StreamInfo = forall a . StreamInfo
  { streamInfoQueue   :: Q.Queue a
  , streamInfoTempVar :: A.V a
  , streamInfoType    :: C.Type a }

type StreamInfoMap = Map C.Id StreamInfo

--------------------------------------------------------------------------------

data ExternInfo = forall a . ExternInfo
  { externInfoVar     :: A.V a
  , externInfoType    :: C.Type a }

type ExternInfoMap = Map C.Name ExternInfo

--------------------------------------------------------------------------------

data LetInfo = forall a . LetInfo
  { letInfoVar        :: A.V a
  , letInfoType       :: C.Type a }

type LetInfoMap = Map C.Name LetInfo

--------------------------------------------------------------------------------

data MetaTable = MetaTable
  { streamInfoMap     :: StreamInfoMap
  , externInfoMap     :: ExternInfoMap
  , letInfoMap        :: LetInfoMap }

--------------------------------------------------------------------------------

allocMetaTable :: C.Spec -> A.Atom MetaTable
allocMetaTable spec =
  do
    streamInfoMap_ <-
      liftM M.fromList $ mapM allocStream (C.specStreams spec)

    externInfoMap_ <-
      liftM M.fromList $ mapM allocExtern (externals spec)

    letInfoMap_ <-
      liftM M.fromList $ return []

    return (MetaTable streamInfoMap_ externInfoMap_ letInfoMap_)

--------------------------------------------------------------------------------

allocStream :: C.Stream -> Atom (C.Id, StreamInfo)
allocStream
  C.Stream
    { C.streamId       = id
    , C.streamBuffer   = buf
    , C.streamExprType = t
    } =
  do
    W.ExprInst <- return (W.exprInst t)
    que <- Q.queue (mkQueueName   id) buf
    tmp <- A.var   (mkTempVarName id) (C.uninitialized t)
    let
      strmInfo =
        StreamInfo
          { streamInfoQueue       = que
          , streamInfoTempVar     = tmp
          , streamInfoType        = t }
    return (id, strmInfo)

--------------------------------------------------------------------------------

allocExtern :: Extern -> Atom (C.Name, ExternInfo)
allocExtern (Extern name t) =
  do
    W.ExprInst <- return (W.exprInst t)
    v <- A.var (mkExternName name) (C.uninitialized t)
    return (name, ExternInfo v t)

--------------------------------------------------------------------------------

{-
allocLet :: C.Let -> Atom (C.Name, LetInfo)
allocLet C.Let
  { C.letVar  = name
  , C.letType = t
  } =
    do
      W.ExprInst <- return (W.exprInst t)
      v <- A.var (mkLetName name) (C.uninitialized t)
      return (name, LetInfo v t)
-}

--------------------------------------------------------------------------------

mkExternName :: C.Name -> A.Name
mkExternName name = "ext_" ++ name

mkLetName :: C.Name -> A.Name
mkLetName name = "let_" ++ name

mkQueueName :: C.Id -> A.Name
mkQueueName id = "s" ++ show id

mkTempVarName :: C.Id -> A.Name
mkTempVarName id = "tmp" ++ show id

--------------------------------------------------------------------------------