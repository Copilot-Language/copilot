{-# LANGUAGE Safe #-}

-- |
-- Copyright: (c) 2011 National Institute of Aerospace / Galois, Inc.
--
-- Custom functions to report error messages to users.
module Copilot.Compile.C99.Error
    ( impossible )
  where

-- | Report an error due to a bug in Copilot.
impossible :: String -- ^ Name of the function in which the error was detected.
           -> String -- ^ Name of the package in which the function is located.
           -> a
impossible function package =
  error $ "Impossible error in function "
    ++ function ++ ", in package " ++ package
    ++ ". Please file an issue at "
    ++ "https://github.com/Copilot-Language/copilot/issues"
    ++ " or email the maintainers at <ivan.perezdominguez@nasa.gov>"
