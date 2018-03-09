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

{- Compile function, currently prints to stdout -}
compile :: Spec -> IO ()
compile s = do
    putStrLn $ render $ foldr ($+$) empty code
    where
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
