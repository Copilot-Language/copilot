--------------------------------------------------------------------------------

module Copilot.Kind.ProofScheme where

import Control.Monad.Writer

--------------------------------------------------------------------------------

type PropId = String

data ProofScheme = ProofScheme [Action] deriving (Show)
  
data Action
  = Check   PropId
  | Assume  PropId
  | Local   [Action]
  | Pragma  Pragma
  deriving (Show)
  
data Pragma = PrintMsg String deriving (Show)

--------------------------------------------------------------------------------

type ProofMonad = Writer [Action]

proof :: ProofMonad () -> ProofScheme
proof = ProofScheme . execWriter

check :: PropId -> ProofMonad ()
check p = tell [Check p]

msg :: String -> ProofMonad ()
msg s = tell [Pragma $ PrintMsg s]

assert :: PropId -> ProofMonad ()
assert p = tell [Check p, Assume p]

assume :: PropId -> ProofMonad ()
assume p = tell [Assume p]

assuming :: [PropId] -> ProofMonad a -> ProofMonad a
assuming ps m = do
  (x, actions) <- listen m
  tell [Local $ map Assume ps ++ actions]
  return x

--------------------------------------------------------------------------------
