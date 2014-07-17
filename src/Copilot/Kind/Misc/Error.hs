--------------------------------------------------------------------------------

module Copilot.Kind.Misc.Error 
  ( badUse
  , impossible
  , impossible_
  , notHandled 
  , fatal
  ) where

--------------------------------------------------------------------------------

errorHeader :: String
errorHeader = "[Copilot-kind ERROR]  "

badUse :: String -> a
badUse s = error $ errorHeader ++ s

impossible :: String -> a
impossible s = error $ errorHeader ++ "Unexpected internal error : " ++ s

impossible_ :: a
impossible_ = error $ errorHeader ++ "Unexpected internal error"

notHandled :: String -> a
notHandled s = error $ errorHeader ++ "Not handled : " ++ s

fatal :: String -> a
fatal = error

--------------------------------------------------------------------------------