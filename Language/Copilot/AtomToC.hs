{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines a main() and print statements to easily execute generated Copilot specs.
module Language.Copilot.AtomToC(getPrePostCode, AtomToC(..)) where

import Language.Copilot.AdHocC
import Language.Copilot.Core

import Language.Atom (Type(Bool), Clock)

import Data.Maybe (fromMaybe)
import Data.List

-- | Datatype corresponding to the 'Options' datatype in 'Interface.hs', but
-- only including the compilation-relevant fields.
data AtomToC = AtomToC 
    { cName :: Name -- ^ Name of the C file to generate
    , gccOpts :: String -- ^ Options to pass to the compiler
    , getPeriod :: Maybe Period -- ^ The optional period
    , outputDir :: String -- ^ Where to put the executable
    , compiler :: String -- ^ Which compiler to use
    , randomProg :: Bool -- ^ Was the program randomly generated?
    , sim :: Bool -- ^ Are we running a C simulator?
    , prePostCode :: (Maybe String, Maybe String) -- ^ Code to replace the default
                                                  -- initialization and main
    , arrDecs :: [(String, Int)] -- ^ When generating C programs to test, we
                                 -- don't know how large external arrays are, so
                                 -- we cannot declare them.  Passing in pairs
                                 -- containing the name of the array and it's
                                 -- size allows them to be declared.
    , clock :: Maybe Clock     -- ^ Use the hardware clock to drive the timing
                               -- of the program.
    }

-- allExts represents all the variables to monitor (used for declaring them)
-- inputExts represents the monitored variables which are to be fed to the
-- standard input of the C program.  only used for the testing with random
-- streams and values.
getPrePostCode :: Bool -> (Maybe String, Maybe String) -> Name 
               -> StreamableMaps Spec -> [Exs] -> [(Ext,Int)] -> SimValues 
               -> Period -> (String, String)
getPrePostCode simulatation (pre, post) cname streams allExts 
               arrdecs inputExts p =
    ( (if simulatation then preCode (extDecls allExts arrdecs)
         else "") ++ fromMaybe "" pre
    , fromMaybe "" post ++ periodLoop cname p 
      ++ if simulatation then (postCode cname streams inputExts)
           else ""
    )

-- Make the declarations for external vars
extDecls :: [Exs] -> [(Ext,Int)] -> [String]
extDecls allExtVars arrdecs =
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
        getIdx arr = lookup arr arrdecs
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

-- | Generate a temporary C file name. 
tmpCFileName :: String -> String
tmpCFileName name = "__" ++ name

periodLoop :: Name -> Period -> String
periodLoop cname p = unlines
  [ "\n"
  , "void " ++ tmpCFileName cname ++ "(void) {"
  , "  int i;"
  , "  for(i = 0; i < " ++ show p ++ "; i++) {"
  , "    "  ++ cname ++ "();"
  , "  }"
  , "}"
  ]

postCode :: Name -> StreamableMaps Spec -> SimValues -> String
postCode cname streams inputExts = 
  unlines $
  [""] ++
  -- (if isEmptySM inputExts
  --    then []
  --    else cleanString)
  -- make a loop to complete a period of computation.
  [ "int main(int argc, char *argv[]) {"
  , "  if (argc != 2) {"
  , "    " ++ printfNewline 
                (   "Please pass a single argument to the simulator"
                 ++ " containing the number of rounds to execute it.")
                []
  , "    return 1;"
  , "  }"
  , "  rnd = strtol(argv[1], NULL, 10); //Yes, we really should do more "
      ++ "error-checking here."
  , "  int i;"
  , "  for(i = 0; i < rnd ; i++) {"
  , "    " ++ printf "period: %i   " ["i"]
  ]
  ++ inputExtVars inputExts "    "
  ++ ["    " ++ tmpCFileName cname ++ "();"]
  ++ outputVars cname streams 
  ++ 
  [ "  }"
  , "  //Important to let the Haskell program know we're done with stdout."
  , "  " ++ printfNewline "" []
  , "  return EXIT_SUCCESS;"
  , "}"
  ]

-- | Get variable values to sample.
inputExtVars :: SimValues -> String -> [String]
inputExtVars exts indent =
    foldStreamableMaps decl exts []
    where
      decl :: Streamable a => Var -> [a] -> [String] -> [String]
      decl v l ls =
        let spec = if null l then error "Impossible error in inputExtVars" 
                     else head l
            (frmt, mMacro) = scnId spec            
            aBool   = atomType spec == Bool
            mTmp    = if aBool then "__tmpCopilotBool_" ++ v else v
            scan    =    indent ++ "scanf(" ++ "\"" ++ frmt ++ "\"" ++ mMacro 
                      ++ ", " ++ "&" ++ mTmp ++ ");" in
        (if aBool 
           then    indent ++"//We can't scanf directly into a Bool, "
                ++ "so we get an int then cast.\n" 
                ++ (indent ++ "int " ++ mTmp ++ ";\n")
                ++ scan ++ "\n"
                ++ indent ++ v ++ " = (bool) " ++ mTmp ++ ";"
              
           else scan) : ls

-- | Print the Copilot stream values to standard out.
outputVars :: Name -> StreamableMaps Spec -> [String]
outputVars cname streams =
    foldStreamableMaps decl streams []
    where
      decl :: forall a. Streamable a => Var -> Spec a -> [String] -> [String]
      decl v _ ls =
        let (frmt, mMacro) = prtIdPrec (unit::a) 
            prtf = printf (v ++ ": " ++ frmt ++ "\" " ++ mMacro ++ "\"   ") 
                          [vPre cname ++ v] in
        ("    " ++ prtf) : ls
