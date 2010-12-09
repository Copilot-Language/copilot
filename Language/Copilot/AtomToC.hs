{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines a main() and print statements to easily execute generated Copilot specs.
module Language.Copilot.AtomToC(getPrePostCode) where

import Language.Copilot.Compiler (tmpSampleStr, tmpArrName, tmpVarName)
import Language.Copilot.AdHocC

import Language.Copilot.Core

import Data.List

-- allExts represents all the variabbles to monitor (used for declaring them)
-- inputExts represents the monitored variables which are to be fed to the
-- standard input of the C program.  only used for the testing with random
-- streams and values.
getPrePostCode :: Name -> StreamableMaps Spec -> [Exs] 
               -> [(Ext,Int)] -> Vars -> Period -> (String, String)
getPrePostCode cName streams allExts arrDecs inputExts p =
    (preCode $ extDecls allExts arrDecs, postCode cName streams allExts inputExts p)

-- Make the declarations for external vars
extDecls :: [Exs] -> [(Ext,Int)] -> [String]
extDecls allExtVars arrDecs =
    let uniqueExtVars = nubBy (\ (x, y, _) (x', y', _) -> x == x' && y == y') 
                              allExtVars 
        getDec :: Exs -> String
        getDec (t, (ExtV v), ExtRetV) = varDecl t [v]
        getDec (t, (Fun f _), ExtRetV) = ""
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
  , includeBracket "inttypes.h"
  , ""
  , "unsigned long long rnd;"
  ]
  ++ extDeclarations
  
postCode :: Name -> StreamableMaps Spec -> [Exs] -> Vars -> Period -> String
postCode cName streams allExts inputExts p = 
  unlines $
  (if isEmptySM inputExts
    then []
    else cleanString)
  ++
  [ "int main(int argc, char *argv[]) {"
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

sampleExtVars :: [Exs] -> Name -> [String]
sampleExtVars allExts cName =
    map (\ext -> let (v,e) = sample ext in
           "  " ++ vPre cName ++ tmpSampleStr ++ (normalizeVar e)
           ++ " = " ++ v ++ ";") 
        allExts
    where 
        sample :: Exs -> (Var, String)
        sample (_, v, ExtRetV) = ( case v of 
                                     ExtV var -> var
                                     Fun fname args -> funcShow cName fname args
                                  , tmpVarName v)
        sample (_, v, ExtRetA idx) = (show v ++ "[0]", tmpArrName v idx)

outputVars :: Name -> StreamableMaps Spec -> [String]
outputVars cName streams =
    foldStreamableMaps decl streams []
    where
        decl :: forall a. Streamable a 
             => Var -> Spec a -> [String] -> [String]
        decl v _ ls =
            ("    " ++ printf (v ++ ": " ++ typeIdPrec (unit::a) ++ "   ") 
            [vPre cName ++ v]) : ls
