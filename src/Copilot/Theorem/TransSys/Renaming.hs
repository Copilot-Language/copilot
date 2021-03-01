--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

-- | A monad capable of keeping track of variable renames and of providing
-- fresh names for variables.
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

-- | A monad capable of keeping track of variable renames and of providing
-- fresh names for variables.
type Renaming = State RenamingST

-- | State needed to keep track of variable renames and reserved names.
data RenamingST = RenamingST
  { _reservedNames :: Set Var
  , _renaming      :: Map ExtVar Var }

--------------------------------------------------------------------------------

-- | Register a name as reserved or used.
addReservedName :: Var -> Renaming ()
addReservedName v = modify $ \st ->
    st {_reservedNames = Set.insert v (_reservedNames st)}

-- | Produce a fresh new name based on the variable names provided.
--
-- This function will try to pick a name from the given list and, if not, will
-- use one of the names in the list as a basis for new names.
--
-- PRE: the given list cannot be empty.
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

-- | Map a name in the global namespace to a new variable name.
rename :: NodeId  -- ^ A node Id
       -> Var     -- ^ A variable within that node
       -> Var     -- ^ A new name for the variable
       -> Renaming ()
rename n v v' = modify $ \st ->
    st {_renaming = Map.insert (ExtVar n v) v' (_renaming st)}

-- | Return a function that maps variables in the global namespace to their new
-- names if any renaming has been registered.
getRenamingF :: Renaming (ExtVar -> Var)
getRenamingF = do
  mapping <- _renaming <$> get
  return $ \extv -> fromMaybe (extVarLocalPart extv) (Map.lookup extv mapping)

-- | Run a computation in the 'Renaming' monad, providing a result and the
-- renaming function that maps variables in the global namespace to their new
-- local names.
runRenaming :: Renaming a -> (a, ExtVar -> Var)
runRenaming m =
  evalState st' (RenamingST Set.empty Map.empty)
  where
    st' = do
      r <- m
      f <- getRenamingF
      return (r, f)

--------------------------------------------------------------------------------
