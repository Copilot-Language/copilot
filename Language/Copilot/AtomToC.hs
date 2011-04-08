{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines a main() and print statements to easily execute generated Copilot specs.
module Language.Copilot.AtomToC(getPrePostCode) where

import Language.Copilot.AdHocC

import Language.Copilot.Core

import Data.Maybe (fromMaybe)
import Data.List

-- allExts represents all the variables to monitor (used for declaring them)
-- inputExts represents the monitored variables which are to be fed to the
-- standard input of the C program.  only used for the testing with random
-- streams and values.
getPrePostCode :: Bool -> (Maybe String, Maybe String) -> Name 
               -> StreamableMaps Spec -> [Exs] -> [(Ext,Int)] -> SimValues 
               -> Period -> (String, String)
getPrePostCode simulatation (pre, post) cName streams allExts 
               arrDecs inputExts p =
    ( (if simulatation then preCode (extDecls allExts arrDecs)
         else "") ++ fromMaybe "" pre
    , fromMaybe "" post ++ periodLoop cName p 
      ++ if simulatation then (postCode cName streams inputExts)
           else ""
    )

-- Make the declarations for external vars
extDecls :: [Exs] -> [(Ext,Int)] -> [String]
extDecls allExtVars arrDecs =
    let uniqueExtVars = nubBy (\ (x, y, _) (x', y', _) -> x == x' && y == y') 
                              allExtVars 
        getDec :: Exs -> String
        getDec (t, (ExtV v), ExtRetV) = varDecl t [v]
        getDec (_, (Fun _ _), ExtRetV) = ""
        getDec (t, arr, ExtRetA _) = 
          case getIdx arr of 
            Nothing -> error $ "Please use the setArrs option to provide a list of " ++
                          "pairs (a,idx) where a is the name of an external array and idx " ++
                          "is its static size to declare.  There is no size for array " ++
                          show arr ++ "."
            Just idx  -> arrDecl t [(show arr, idx)] 
        getIdx arr = lookup arr arrDecs
    in 
    map getDec uniqueExtVars

preCode :: [String] -> String
preCode extDeclarations = unlines $
  [ includeBracket "stdio.h"
  , includeBracket "stdlib.h"
  , includeBracket "string.h"
--  , includeBracket "inttypes.h"
  , ""
  , "unsigned long long rnd;"
  ]
  ++ extDeclarations

-- | Generate a temporary C file name. 
tmpCFileName :: String -> String
tmpCFileName name = "__" ++ name

periodLoop :: Name -> Period -> String
periodLoop cName p = unlines
  [ "\n"
  , "void " ++ tmpCFileName cName ++ "(void) {"
  , "  int i;"
  , "  for(i = 0; i < " ++ show p ++ "; i++) {"
  , "    "  ++ cName ++ "();"
  , "  }"
  , "}"
  ]

postCode :: Name -> StreamableMaps Spec -> SimValues -> String
postCode cName streams inputExts = 
  unlines $
  [""] ++
  (if isEmptySM inputExts
     then []
     else cleanString)
  ++ -- make a loop to complete a period of computation.
  [ "int main(int argc, char *argv[]) {"
  , "  if (argc != 2) {"
  , "    " ++ printfNewline 
         "Please pass a single argument to the simulator containing the number of rounds to execute it." 
         []
  , "    return 1;"
  , "  }"
  , "  rnd = atoi(argv[1]);"
  , "  int i;"
  , "  for(i = 0; i < rnd ; i++) {"
  , "    " ++ printf "period: %i   " ["i"]
  ]
  ++ inputExtVars inputExts "    "
  ++ ["    " ++ tmpCFileName cName ++ "();"]
  ++ outputVars cName streams 
  ++ 
  [ "    " ++ printfNewline "" []
  , "    fflush(stdout);"
  , "  }"
  , "  return EXIT_SUCCESS;"
  , "}"
  ]
  where
    cleanString =
        [ "void clean(const char *buffer, FILE *fp) {"
        , "  char *p = strchr(buffer,'\\n');"
        , "  if (p != NULL)"
        , "    *p = 0;"
        , "  else {"
        , "    int c;"
        , "    while ((c = fgetc(fp)) != '\\n' && c != EOF);"
        , "  }"
        , "}"
        , ""
        ]

inputExtVars :: SimValues -> String -> [String]
inputExtVars exts indent =
    foldStreamableMaps decl exts []
    where
        decl :: Streamable a => Var -> [a] -> [String] -> [String]
        decl v l ls =
            let string = "string_" ++ v in
            (indent ++ "char " ++ string ++ " [50] = \"\";") :
            (indent ++ "fgets (" ++ string ++ ", sizeof(" ++ string 
                    ++ "), stdin);") :
            (indent ++ "sscanf (" ++ string ++ ", \"" 
                    ++ typeId (head l) ++ "\", &" ++ v ++ ");") :
            (indent ++ "clean (" ++ string ++ ", stdin);") : ls

outputVars :: Name -> StreamableMaps Spec -> [String]
outputVars cName streams =
    foldStreamableMaps decl streams []
    where
        decl :: forall a. Streamable a 
             => Var -> Spec a -> [String] -> [String]
        decl v _ ls =
            ("    " ++ printf (v ++ ": " ++ typeIdPrec (unit::a) ++ "   ") 
            [vPre cName ++ v]) : ls
