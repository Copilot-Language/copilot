module Copilot.Backend.C.Compile where

import Copilot.Backend.C.CodeGen
import Copilot.Backend.C.Normalize

import Copilot.Core (Spec)
import Copilot.Core.PrettyPrint

import Language.C99.AST ( TransUnit (..)
                        , ExtDecln (..)
                        )
import Language.C99.Pretty (pretty)

import Text.PrettyPrint (render)

import Control.Monad.State (execState)

{- A function to test compilation -}
testcompile :: Spec -> IO ()
testcompile s = do
  let s' = normalize s
  putStrLn $ prettyPrint s
  putStrLn dots
  compile s
  putStrLn line
  putStrLn $ prettyPrint s'
  putStrLn dots
  compile s'
  where
    dots = ". . . . . . . . . ."
    line = "-------------------"

{- Compile function, currently prints to stdout -}
compile :: Spec -> IO ()
compile s = do
    putStrLn $ unlines headers
    putStrLn $ render $ pretty (compile' s)
    where
      headers = [ "#include <stdio.h>"
                , "#include <stdbool.h>"
                , "#include <string.h>"
                , "#include <stdint.h>"
                ]

      compile' :: Spec -> TransUnit
      compile' s = TransUnit (vars' ++ funcs') where
        vars' = map EDDecln (vars defs)
        funcs' = map EDFunDef (funcs defs)

        defs = execState (codegen s) emptyProgState
