--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}

-- | Custom functions to report error messages to users.
module Copilot.Theorem.Misc.Error
  ( badUse
  , impossible
  , impossible_
  , fatal
  ) where

--------------------------------------------------------------------------------

-- | Tag used with error messages to help users locate the component that
-- failed or reports the error.
errorHeader :: String
errorHeader = "[Copilot-kind ERROR]  "

-- | Report an error due to an error detected by Copilot (e.g., user error).
badUse :: String -- ^ Description of the error.
       -> a
badUse s = error $ errorHeader ++ s

-- | Report an error due to a bug in Copilot.
impossible :: String -- ^ Error information to attach to the message.
           -> a
impossible s = error $ errorHeader ++ "Unexpected internal error : " ++ s

-- | Report an error due to a bug in Copilot.
impossible_ :: a
impossible_ = error $ errorHeader ++ "Unexpected internal error"

-- | Report an unrecoverable error (e.g., incorrect format).
fatal :: String -> a
fatal = error

--------------------------------------------------------------------------------
