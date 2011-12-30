--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}

module Copilot.Compile.C99.PrePostCode
  ( preCode , postCode
  ) where

import Copilot.Core
import Copilot.Compile.C99.Common (typeSpec, mkTmpExtFunVarName)
import Copilot.Compile.C99.Params
import Copilot.Compile.C99.Phases (numberOfPhases)
import Copilot.Compile.Header.C99 (c99HeaderName)

--------------------------------------------------------------------------------

preCode :: Params -> Spec -> String
preCode params spec = unlines $
  [ "#include \"" ++ c99HeaderName (prefix params) ++ "\"" ] ++
  ( map (observerDecl params) . specObservers ) spec ++
  ( map (tmpExtFunVar params) . externFuns    ) spec

--------------------------------------------------------------------------------

observerDecl :: Params -> Observer -> String
observerDecl params (Observer cs _ t) = typeSpec t ++ " " ++ name ++ ";"
  where name = withPrefix (prefix params) cs

--------------------------------------------------------------------------------

tmpExtFunVar :: Params -> ExtFun -> String
tmpExtFunVar _ ExtFun
  { externFunName = name
  , externFunType = t
  , externFunTag  = mtag } =
    case mtag of
      Nothing  -> impossible "tmpExtFunVar" "copilot-c99"
      Just tag -> "static " ++ typeSpec t ++ " " 
                    ++ mkTmpExtFunVarName name tag ++ ";"

--------------------------------------------------------------------------------

postCode :: Params -> Spec -> String
postCode params _ =
  unlines
    [ "void " ++ withPrefix (prefix params) "step" ++ "()"
    , "{"
    , "  " ++ concat (replicate numberOfPhases step)
    , "}"
    ]

  where step = withPrefix (prefix params) "copilot" ++ "();"
  
