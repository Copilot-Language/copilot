--------------------------------------------------------------------------------

module Copilot.Kind.TransSys.Renaming 
  ( Renaming
  , addReservedName
  , rename
  , getFreshName
  , runRenaming
  ) where

import Copilot.Kind.Misc.Utils
import Copilot.Kind.TransSys.Spec

import Control.Monad.State.Lazy

import qualified Data.Map as Map
import qualified Data.Set as Set

--------------------------------------------------------------------------------

newtype Renaming a = Renaming (State RenamingST a) 
                     deriving (Monad, Functor)

data RenamingST = RenamingST
  { _reservedNames :: Set LVar
  , _renaming      :: Map GVar LVar }

--------------------------------------------------------------------------------

addReservedName :: LVar -> Renaming ()
addReservedName v = 
  Renaming $ modify $ \st -> 
    st {_reservedNames = Set.insert v (_reservedNames st)}


getFreshName :: [LVar] -> Renaming LVar
getFreshName vs = do
  usedNames <- _reservedNames <$> Renaming get
  let v = case dropWhile (`member` usedNames) vs of
            v:_ -> v
            [] -> error "No more names available"
  addReservedName v
  return v 

rename :: NodeId -> LVar -> LVar -> Renaming ()
rename n v v' = 
  Renaming $ modify $ \st ->
    st {_renaming = Map.insert (GVar n v) v' (_renaming st)}

runRenaming :: Renaming a -> (a, GVar -> LVar)
runRenaming (Renaming st) = (x, renamingF)
  where (x, s) = runState st $ RenamingST Set.empty Map.empty
        renamingF gv = case Map.lookup gv (_renaming s) of
          Just v' -> v'
          Nothing -> localPart gv
           
--------------------------------------------------------------------------------
