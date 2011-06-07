-- generate C-Code with combinators, high-level, safe haskell.

-- | Helper functions for writing free-form C code.
module Language.Copilot.AdHocC (
         varDecl, arrDecl, varInit, arrayInit, funcDecl
       , includeBracket, includeQuote, printf, printfNewline
    ) where

import Data.List (intersperse)
import Language.Atom (Type(..))
import Language.Atom.Code (cType) -- C99

-- | Takes a type and a list of variable names and declares them.
varDecl :: Type -> [String] -> String
varDecl t vars =
    cType t ++ " " ++ unwords (intersperse "," vars) ++ ";"

-- | Takes a type and a list of array names and their sizes declares them.
arrDecl :: Type -> [(String, Int)] -> String
arrDecl t arrs =
    cType t ++ " " ++ unwords (intersperse "," mkArrs) ++ ";"
  where mkArrs = map (\(a,size) -> a ++ "[" ++ show size ++ "]") arrs

-- | Takes a type and a variable and initializes it.  It is YOUR responsibility
-- to ensure that @val@ is of type @t@.
varInit :: Show a => Type -> String -> a -> String
varInit t var val = cType t ++ " " ++ var ++ " = " ++ show val ++ ";"

-- | Takes a type and an array and initializes it.  It is YOUR responsibility to
-- ensure that @vals@ is of type @t@.
arrayInit :: Show a => Type -> String -> [a] -> String
arrayInit t var vals =
  cType t ++ " " ++ var ++ "[" ++ show (length vals)
        ++ "] = " ++ bracesListShow ++ ";"
  where
  -- Show a list with braces {} rather than brackets [].
  bracesListShow :: String
  bracesListShow =
    "{" ++ (foldl (++) "" $ intersperse "," $ map show vals) ++ "}"

-- | Declare function prototypes, given a return type, a function name, and a
-- | list of argument types.  Use 'Nothing' for a return type of @void@.
funcDecl :: Maybe Type -> String -> [Type] -> String
funcDecl t fn argTs = makeRet t ++ " " ++ fn ++ "(" ++ makeArgTs ++ ");"
  where makeRet Nothing = "void"
        makeRet (Just t') = cType t'
        makeArgTs | null argTs = "void"
                  | otherwise  = unwords (intersperse "," $ map cType argTs)

-- | Add an include of a library
includeBracket :: String -> String
includeBracket lib = "#include <" ++ lib ++ ">"

-- | Add an include of a header file
includeQuote :: String -> String
includeQuote lib = "#include \"" ++ lib ++ "\""

printfPre :: String -> String
printfPre = ("printf(\"" ++)

printfPost :: [String] -> String
printfPost vars =
  let sep = if null vars then " " else ", "
  in "\"" ++ sep ++ unwords (intersperse "," vars) ++ ");"

newline :: String
newline = "\\n"

-- | printf, with and without a newline (nl) character.
printf, printfNewline :: String -> [String] -> String
printfNewline text vars = (printfPre text) ++ newline ++ (printfPost vars)
printf text vars = (printfPre text) ++ (printfPost vars)

