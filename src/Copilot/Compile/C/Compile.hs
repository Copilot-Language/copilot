module Copilot.Compile.C.Compile where

import Copilot.Compile.C.CodeGen
import Copilot.Compile.C.Normalize

import Copilot.Compile.ACSL.CodeGen

import Copilot.Core (Spec)

import Language.C99.Pretty (pretty)

import Data.List (intersperse)
import Text.PrettyPrint ( render
                        , ($+$)
                        , (<>)
                        , semi
                        , empty
                        , text
                        , Doc
                        , doubleQuotes )

import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (normalise)


{- Apply the given prefix to a base filename -}
applyprefix :: String -> String -> String
applyprefix ""  filename = filename
applyprefix pre filename = pre ++ "_" ++ filename

{- seperate with whitelines -}
seperate :: [Doc] -> [Doc]
seperate ds = intersperse (text "") ds


ccode :: Spec -> String -> String
ccode s hfile = render $ foldr ($+$) empty code where
  defs = reify $ gather $ normalize s
  code =  [ text "#include <stdio.h>"
          , text "#include <string.h>"
          , text "#include <math.h>"
          , text ""
          , text "#include " <> doubleQuotes (text hfile)
          , text ""
          ]
          ++
          map (\x -> pretty x <> semi) (vars defs)
          ++
          [ text "" ]
          ++
          seperate funs
          ++
          [ text "" ]
  acsl = acslgen $ gather s
  funs :: [Doc]
  funs = map (\(f,d) -> d $+$ pretty f) (zip (funcs defs) acsl)


hcode :: Spec -> String
hcode s = render $ foldr ($+$) empty code where
  (vars, triggers, step) = headerfile $ gather s
  code =  [ text "#include <stdbool.h>"
          , text "#include <stdint.h>"
          ]
          ++
          [ text ""
          , text "/* External variables */" ] ++
          map pretty vars
          ++
          [ text ""
          , text "/* Triggers */" ] ++
          map pretty triggers
          ++
          [ text "" ] ++
          [ pretty step ] ++
          [ text "" ]


{- Compile function, writes both .c as well as *.h file -}
compile :: String -> FilePath -> Spec -> IO ()
compile prefix outdir s = do
  createDirectoryIfMissing True (normalise out)
  writeFile cpath (ccode s hfile)
  writeFile hpath (hcode s)
  where
    basename = applyprefix prefix "monitor"
    cfile    = basename ++ ".c"
    hfile    = basename ++ ".h"
    cpath    = normalise $ out ++ cfile
    hpath    = normalise $ out ++ hfile

    out = outdir ++ "/"
