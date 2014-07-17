--------------------------------------------------------------------------------

module Copilot.Kind.ProofScheme where

import Control.Monad.Writer

--------------------------------------------------------------------------------

type PropId = String

type ProofScheme = Writer [Action] ()
  
data Action
  = Check   PropId
  | Assume  PropId
  | Local   [Action]
  | Pragma  Pragma
  deriving (Show)
  
data Pragma = PrintMsg String deriving (Show)

--------------------------------------------------------------------------------

check :: PropId -> ProofScheme
check p = tell [Check p]

msg :: String -> ProofScheme
msg s = tell [Pragma $ PrintMsg s]

assert :: PropId -> ProofScheme
assert p = tell [Check p, Assume p]

assume :: PropId -> ProofScheme
assume p = tell [Assume p]

assuming :: [PropId] -> ProofScheme -> ProofScheme
assuming ps m = censor wrap m
  where wrap actions = [Local $ map Assume ps ++ actions]

--------------------------------------------------------------------------------
