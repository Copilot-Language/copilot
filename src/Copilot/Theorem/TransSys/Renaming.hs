--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

module Copilot.Theorem.TransSys.Renaming
  ( Renaming
  , addReservedName
  , rename
  , getFreshName
  , runRenaming
  , getRenamingF
  ) where

import Copilot.Theorem.TransSys.Spec

import Control.Monad.State.Lazy

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Set (Set, member)

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.List as List

--------------------------------------------------------------------------------

type Renaming = State RenamingST

data RenamingST = RenamingST
  { _reservedNames :: Set Var
  , _renaming      :: Map ExtVar Var }

--------------------------------------------------------------------------------

addReservedName :: Var -> Renaming ()
addReservedName v = modify $ \st ->
    st {_reservedNames = Set.insert v (_reservedNames st)}

getFreshName :: [Var] -> Renaming Var
getFreshName vs = do
  usedNames <- _reservedNames <$> get
  let varAppend (Var s) = Var $ s ++ "_"
      applicants = vs ++ List.iterate varAppend (head vs)
      v = case dropWhile (`member` usedNames) applicants of
            v:_ -> v
            [] -> error "No more names available"
  addReservedName v
  return v

rename :: NodeId -> Var -> Var -> Renaming ()
rename n v v' = modify $ \st ->
    st {_renaming = Map.insert (ExtVar n v) v' (_renaming st)}

getRenamingF :: Renaming (ExtVar -> Var)
getRenamingF = do
  mapping <- _renaming <$> get
  return $ \extv -> fromMaybe (extVarLocalPart extv) (Map.lookup extv mapping)

runRenaming :: Renaming a -> (a, ExtVar -> Var)
runRenaming m =
  evalState st' (RenamingST Set.empty Map.empty)
  where
    st' = do
      r <- m
      f <- getRenamingF
      return (r, f)

--------------------------------------------------------------------------------
