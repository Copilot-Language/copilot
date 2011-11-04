--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.C99.Params (Params (..), defaultParams, withPrefix) where

data Params = Params
  { prefix  :: Maybe String -- An string to prefix the output with
  , verbose :: Bool 
  }

defaultParams :: Params
defaultParams = Params
  { prefix  = Nothing
  , verbose = True
  }

withPrefix :: Maybe String -> String -> String
withPrefix (Just cs) ds = cs ++ "_" ++ ds
withPrefix _         ds = ds
