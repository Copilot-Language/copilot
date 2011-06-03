--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Copilot.Compile.C99.MetaTable
  ( StreamInfo (..)
  , ExternInfo (..)
  , StreamInfoMap
  , ExternInfoMap
  , MetaTable (..)
  , allocMetaTable
  ) where

import Control.Monad (liftM, liftM2)
import qualified Copilot.Compile.C99.Queue as Q
import qualified Copilot.Compile.C99.Witness as W
import qualified Copilot.Core as C
import Copilot.Core.Uninitialized (uninitialized)
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

type StreamInfoMap = Map C.Id   StreamInfo

--------------------------------------------------------------------------------

data ExternInfo = forall a . ExternInfo
  { externInfoVar     :: A.V a
  , externInfoType    :: C.Type a }

type ExternInfoMap = Map C.Name ExternInfo

--------------------------------------------------------------------------------

data MetaTable = MetaTable
  { streamInfoMap     :: StreamInfoMap
  , externInfoMap     :: ExternInfoMap }

--------------------------------------------------------------------------------

allocMetaTable :: C.Spec -> A.Atom MetaTable
allocMetaTable spec =
  do
    streamInfoMap_ <-
      liftM M.unions $ mapM allocStream (C.specStreams spec)

    externInfoMap_ <-
      liftM M.unions $ liftM2 (++)
        ( mapM allocExtsInStream  (C.specStreams spec)  )
        ( mapM allocExtsInTrigger (C.specTriggers spec) )

    return (MetaTable streamInfoMap_ externInfoMap_)

  where

  allocExtsInStream :: C.Stream -> Atom ExternInfoMap
  allocExtsInStream (C.Stream _ _ _ e _) = allocExts e

  allocExtsInTrigger :: C.Trigger -> Atom ExternInfoMap
  allocExtsInTrigger (C.Trigger _ e1 args) =
    liftM2 M.union (allocExts e1) $
      liftM M.unions (mapM allocExtsInTriggerArg args)

  allocExtsInTriggerArg :: C.TriggerArg -> Atom ExternInfoMap
  allocExtsInTriggerArg (C.TriggerArg e _) = allocExts e

--------------------------------------------------------------------------------

allocStream :: C.Stream -> Atom StreamInfoMap
allocStream (C.Stream id buf _ _ t) =
  do
    W.ExprInst <- return (W.exprInst t)
    que <- Q.queue (mkQueueName   id) buf
    tmp <- A.var   (mkTempVarName id) (uninitialized t)
    let
      strmInfo =
        StreamInfo
          { streamInfoQueue       = que
          , streamInfoTempVar     = tmp
          , streamInfoType        = t }
    return $ M.singleton id strmInfo

--------------------------------------------------------------------------------

newtype AllocExts a = AllocExts { allocExts :: Atom ExternInfoMap }

instance C.Expr AllocExts where

  const _ _      = AllocExts $ return M.empty
  drop _ _ _     = AllocExts $ return M.empty
  extern t name  = AllocExts $
    do
      W.ExprInst <- return (W.exprInst t)
      v <- A.var (mkExternName name) (uninitialized t)
      return $ M.singleton name $ ExternInfo v t
  op1 _ e        = AllocExts $ allocExts e
  op2 _ e1 e2    = AllocExts $
    liftM2 M.union (allocExts e1) (allocExts e2)
  op3 _ e1 e2 e3 = AllocExts $
    liftM M.unions $ sequence
      [ allocExts e1, allocExts e2, allocExts e3 ]

--------------------------------------------------------------------------------

mkExternName :: C.Name -> A.Name
mkExternName name = "ext_" ++ name

mkQueueName :: C.Id -> A.Name
mkQueueName id = "s" ++ show id

mkTempVarName :: C.Id -> A.Name
mkTempVarName id = "tmp" ++ show id

--------------------------------------------------------------------------------