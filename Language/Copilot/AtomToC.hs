{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines a main() and print statements to easily execute generated Copilot specs.
module Language.Copilot.AtomToC(getPrePostCode) where

import Language.Copilot.Compiler (tmpSampleStr)
import Language.Copilot.AdHocC
import Language.Copilot.Core
import Language.Copilot.Analyser (ExtVars(..))

import qualified Language.Atom as A

import Data.List

-- allExts represents all the variables to monitor (used for declaring them)
-- inputExts represents the monitored variables which are to be feed to the standard input of the C program.
-- only used for the testing with random streams and values.
getPrePostCode :: Name -> StreamableMaps Spec -> [(A.Type, Var, ExtVars)] 
               -> [(String,Int)] -> Vars -> Period -> (String, String)
getPrePostCode cName streams allExts arrDecs inputExts p =
    (preCode $ extDecls allExts arrDecs, postCode cName streams allExts inputExts p)

-- Make the declarations for external vars
extDecls :: [(A.Type, Var, ExtVars)] -> [(String,Int)] -> [String]
extDecls allExtVars arrDecs =
    let uniqueExtVars = nubBy (\ (x, y, _) (x', y', _) -> x == x' && y == y') allExtVars 
        getDec (t, v, ExtV _) = varDecl t [v]
        getDec (t, arr, ExtA _ _) = 
          case getIdx arr of 
            Nothing -> error $ "Please use the setArrs option to provide a list of " ++
                          "pairs (a,idx) where a is the name of an external array and idx " ++
                          "is its static size to declare.  There is no size for array " ++
                          arr ++ "."
            Just idx  -> arrDecl t [(arr, idx)] 
        getIdx arr = lookup arr arrDecs
    in 
    map getDec uniqueExtVars

preCode :: [String] -> String
preCode extDeclarations = unlines $
  [ includeBracket "stdio.h"
  , includeBracket "stdlib.h"
  , includeBracket "string.h"
  , includeBracket "inttypes.h"
  , ""
  , "unsigned long long rnd;"
  ]
  ++ extDeclarations
  
vPre :: Name -> String
vPre cName = "copilotState" ++ cName ++ "." ++ cName ++ "."

postCode :: Name -> StreamableMaps Spec -> [(A.Type, Var, ExtVars)] -> Vars -> Period -> String
postCode cName streams allExts inputExts p = 
  unlines $
  (if isEmptySM inputExts
    then []
    else cleanString)
  ++
  [ "// #pragma GCC diagnostic ignored \"-Wformat\""
  , "int main(int argc, char *argv[]) {"
  , "  if (argc != 2) {"
  , "    " ++ printfNewline 
         "Please pass a single argument to the simulator containing the number of rounds to execute it." 
         []
  , "    return 1;"
  , "  }"
  , "  rnd = atoi(argv[1]);"
  ]
  ++
  inputExtVars inputExts "  "
  ++
  sampleExtVars allExts cName
  ++
  [ "  int i = 0;"
  , "  for(; i < rnd ; i++) {"
  ]
  ++
  inputExtVars inputExts "    "
  ++
  [ "    int j = 0;"
  , "    for (; j < " ++ show p ++ " ; j++) {"
  , "      " ++ cName ++ "();"
  , "    }"
  , "    " ++ printf "period: %i   " ["i"]
  ]
  ++
  outputVars cName streams 
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

inputExtVars :: Vars -> String -> [String]
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

sampleExtVars :: [(A.Type, Var, ExtVars)] -> Name -> [String]
sampleExtVars allExts cName =
    map (\ext -> let (v,e) = sample ext in
           "  " ++ vPre cName ++ tmpSampleStr ++ e
           ++ " = " ++ v ++ ";") 
        allExts
    where 
        sample :: (A.Type, Var, ExtVars) -> (Var, String)
        sample (_, v, ExtV ph) = (v, tmpVarName v ph)
        sample (_, v, ExtA ph idx) = (v ++ "[0]", tmpArrName v ph idx)

outputVars :: Name -> StreamableMaps Spec -> [String]
outputVars cName streams =
    foldStreamableMaps decl streams []
    where
        decl :: forall a. Streamable a 
             => Var -> Spec a -> [String] -> [String]
        decl v _ ls =
            ("    " ++ printf (v ++ ": " ++ typeIdPrec (unit::a) ++ "   ") 
            [vPre cName ++ v]) : ls
