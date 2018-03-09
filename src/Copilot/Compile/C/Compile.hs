module Copilot.Compile.C.Compile where

import Copilot.Compile.C.CodeGen
import Copilot.Compile.C.Normalize

import Copilot.Core (Spec)
import Copilot.Core.PrettyPrint

import Language.C99.Pretty (pretty)

import Data.List (intersperse)
import Text.PrettyPrint ( render
                        , ($+$)
                        , (<>)
                        , semi
                        , empty
                        , text
                        , Doc )


data Params = Params
  { prefix  :: Maybe String
  }

defaultParams :: Params
defaultParams = Params
  { prefix = Nothing
  }

applyprefix :: Maybe String -> String -> String
applyprefix (Just pre) filename = pre ++ "_" ++ filename
applyprefix _          filename = filename

{- Compile function, currently prints to stdout -}
compile :: Params -> Spec -> IO ()
compile params s = do
    writeFile filename prettycode
    where
      filename   = applyprefix (prefix params) "monitor.c"
      prettycode = render $ foldr ($+$) empty code

      defs = codegen s
      code =  [ text "#include <stdio.h>"
              , text "#include <stdbool.h>"
              , text "#include <string.h>"
              , text "#include <stdint.h>"
              , text ""
              ]
              ++
              map (\x -> pretty x <> semi) (vars defs)
              ++
              [ text "" ]
              ++
              seperate funs

      funs :: [Doc]
      funs = map (\(d,f) -> d $+$ pretty f) (funcs defs)

      {- Seperate with whitelines -}
      seperate :: [Doc] -> [Doc]
      seperate ds = intersperse (text "") ds

