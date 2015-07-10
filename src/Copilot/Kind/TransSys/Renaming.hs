--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Copilot.Kind.TransSys.Renaming
  ( Renaming
  , addReservedName
  , rename
  , getFreshName
  , runRenaming
  , getRenamingF
  ) where

import Copilot.Kind.TransSys.Spec

import Control.Monad.State.Lazy
import Control.Applicative

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Set (Set, member)

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.List as List

--------------------------------------------------------------------------------

newtype Renaming a = Renaming (State RenamingST a)
                     deriving (Applicative, Monad, Functor)

data RenamingST = RenamingST
  { _reservedNames :: Set Var
  , _renaming      :: Map ExtVar Var }

--------------------------------------------------------------------------------

addReservedName :: Var -> Renaming ()
addReservedName v =
  Renaming $ modify $ \st ->
    st {_reservedNames = Set.insert v (_reservedNames st)}


getFreshName :: [Var] -> Renaming Var
getFreshName vs = do
  usedNames <- _reservedNames <$> Renaming get
  let varAppend (Var s) = Var $ s ++ "_"
      applicants = vs ++ List.iterate varAppend (head vs)
      v = case dropWhile (`member` usedNames) applicants of
            v:_ -> v
            [] -> error "No more names available"
  addReservedName v
  return v

rename :: NodeId -> Var -> Var -> Renaming ()
rename n v v' =
  Renaming $ modify $ \st ->
    st {_renaming = Map.insert (ExtVar n v) v' (_renaming st)}

getRenamingF :: Renaming (ExtVar -> Var)
getRenamingF = do
  mapping <- _renaming <$> Renaming get
  return $ \extv -> fromMaybe (extVarLocalPart extv) (Map.lookup extv mapping)

runRenaming :: Renaming a -> (a, ExtVar -> Var)
runRenaming m =
  evalState st' (RenamingST Set.empty Map.empty)
  where
    Renaming st' = do
      r <- m
      f <- getRenamingF
      return (r, f)

--------------------------------------------------------------------------------
