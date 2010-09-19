-- TODO-Robin : Improve a lot that -> new package in Hackage ?
-- generate C-Code with combinators, high-level, safe haskell.

-- | Helper functions for writing free-form C code.
module Language.Copilot.AdHocC (
        varDecl, includeBracket, includeQuote,
        printf, printfNewline
    ) where

import Data.List (intersperse) 
import Language.Atom (Type(..))
import Language.Atom.Code (cType) -- C99

-- | Takes a type and a list of variable names and declares them.
varDecl :: Type -> [String] -> String
varDecl t vars = 
    cType' t ++ " " ++ (unwords (intersperse "," vars)) ++ ";"
    where
        cType' Bool = "int"
        cType' typ = cType typ


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
