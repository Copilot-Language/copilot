{-# LANGUAGE NoMonomorphismRestriction #-}

module RegExp where


import Text.ParserCombinators.Parsec
import Data.Int
import Data.Word
import Data.List
import Data.Char

import Debug.Trace
import Text.Printf

import Language.Copilot.Core
import qualified Language.Copilot.Language as C
import Language.Copilot.Interface


-- The symbols in a regular expression, "Any" is any value of type t
-- (matches any symbol, the "point" character in a regular expression).
-- Start is used only for a start state in the generated NFA, that
-- does not consume any input symbol
data Sym t = Start | Any | Sym t
             deriving ( Eq, Ord )

instance Show t => Show ( Sym t ) where
    show   Start   = "start"
    show   Any     = "any"
    show ( Sym s ) = show s

-- A symbol's value can occur multiple times in a regular expression,
-- e.g. "(tfft)*". A running number "symbolNum" is used to make all
-- symbols in a regular expression unique.
data NumSym t = NumSym { symbolNum :: Int
                       , symbol    :: Sym t
                       } deriving ( Eq )

instance Show t => Show ( NumSym t ) where
    show s     = show ( symbol s ) ++ "_" ++ show ( symbolNum s )


-- The regular expression data type. For our use
-- regular expressions describing a language with
-- no word is not supported since empty languages
-- would not match anything and just yield a
-- copilot stream of constant false values.
data RegExp t = REpsilon
              | RSymbol  ( NumSym t )
              | ROr      ( RegExp t ) ( RegExp t )
              | RConcat  ( RegExp t ) ( RegExp t )
              | RStar    ( RegExp t )
                deriving Show


-- Parsers for single characters.
lquote = char '<'
rquote = char '>'
lparen = char '('
rparen = char ')'
star   = char '*'
point  = char '.'
minus  = char '-'


-- A "followedBy" combinator for parsing, parses
-- p, then p' and returns the result of p.
followedBy p p' = p >>= \ r -> p' >> return r

-- Parsing a string p' with the character p as an
-- optional prefix, return the result with the
-- optional prefix.
optCPrefix p p' = optionMaybe p
                  >>= \ r -> case r of
                               Nothing -> p'
                               Just r  -> p' >>= return . ( r: )

-- The ci function ("case insensitive") takes one argument of
-- type string, parses for the string in a case insensitive
-- manner and yields the parsed string (preserving its case).
ci = mapM ( \ c -> ( char . toLower ) c <|> ( char . toUpper ) c )


-- the parser for regular expressions
regexp  = chainr1 term opOr

term    = chainr factor opConcat REpsilon

factor  = opStar factor'

factor' = between lparen rparen regexp
          <|> anySym
          <|> parseSym

-- Parses the "." - point character used to match any symbol.
anySym  = do { point
             ; symbolNum <- getState
             ; updateState ( +1 )
             ; return $ RSymbol ( NumSym symbolNum Any )
             }

class SymbolParser t where
    parseSym :: GenParser Char Int ( RegExp t )

instance SymbolParser Bool where
    parseSym = do { truth <- ( ci "t" >> ( optional $ ci "rue" )
                               >> return True )
                              <|> ( ci "f" >> ( optional $ ci "alse" )
                                    >> return False )
                              <|> ( string "1" >> return True )
                              <|> ( string "0" >> return False )
                  ; symbolNum <- getState
                  ; updateState ( +1 )
                  ; return $ RSymbol ( NumSym symbolNum $ Sym truth )
                  }

-- TODO: use the SymbolParser class to parse all integral
-- Copilot types by having one parser for signed and one
-- for unsigned values, bound checking
instance SymbolParser Word8 where
    parseSym = do { result <- between lquote rquote $ many1 digit
                  ; symbolNum <- getState
                  ; updateState ( +1 )
                  ; return . RSymbol . ( NumSym symbolNum . Sym ) . read $ result
                  }

instance SymbolParser Int8 where
    parseSym = do { result <- between lquote rquote $
                              optCPrefix minus ( many1 digit )
                  ; symbolNum <- getState
                  ; updateState ( +1 )
                  ; return . RSymbol . ( NumSym symbolNum . Sym ) . read $ result
                  }


opOr     = char '|' >> return ROr
opConcat = return RConcat
opStar r = do { exp <- r
              ; st  <- many star
              ; return $
                if null st then exp else RStar exp
              }

start = regexp `followedBy` eof


hasEpsilon   REpsilon         = True
hasEpsilon ( RSymbol  _     ) = False
hasEpsilon ( ROr      r1 r2 ) = hasEpsilon r1 || hasEpsilon r2
hasEpsilon ( RConcat  r1 r2 ) = hasEpsilon r1 && hasEpsilon r2
hasEpsilon ( RStar    _     ) = True


first   REpsilon         = []
first ( RSymbol  s     ) = [ s ]
first ( ROr      r1 r2 ) = first r1 ++ first r2
first ( RConcat  r1 r2 ) = first r1 ++ if hasEpsilon r1 then
                                           first r2 else []
first ( RStar    r     ) = first r


reverse' ( ROr     r1 r2 ) = ROr     ( reverse' r1 ) ( reverse' r2 )
reverse' ( RConcat r1 r2 ) = RConcat ( reverse' r2 ) ( reverse' r1 )
reverse' ( RStar   r     ) = RStar   ( reverse' r  )
reverse'   e               = e


last' = first . reverse'


follow   REpsilon         _   = []
follow ( RSymbol  _     ) _   = []
follow ( ROr      r1 r2 ) sNr = follow r1 sNr ++ follow r2 sNr
follow ( RConcat  r1 r2 ) sNr = follow r1 sNr ++ follow r2 sNr
                                ++ if sNr `elem` ( last' r1 ) then
                                       first r2 else []
follow ( RStar    r     ) sNr = follow r sNr
                                `union` if sNr `elem` ( last' r ) then
                                            first r else []


preceding = follow . reverse'


hasFinitePath ( ROr     r1 r2 ) = hasFinitePath r1 || hasFinitePath r2
hasFinitePath ( RConcat _  r2 ) = hasFinitePath r2
hasFinitePath ( RStar   r     ) = hasEpsilon r
hasFinitePath   _               = True


getSymbols ( RSymbol s     ) = [ s ]
getSymbols ( ROr     r1 r2 ) = getSymbols r1 ++ getSymbols r2
getSymbols ( RConcat r1 r2 ) = getSymbols r1 ++ getSymbols r2
getSymbols ( RStar   r     ) = getSymbols r
getSymbols   _               = []


-- TODO: get the reset right
regexp2CopilotNFA inStream regexp outStream reset =
    let symbols                    = getSymbols regexp
        ref                        = C.var . show
        startRef                   = ref "start"
        startStream                = startRef C..= reset
        outStream'                 = outStream C..=
                                     foldl ( C.|| ) reset ( map ref symbols )

        preceding'   regexp numSym = case preceding regexp numSym of
                                       []    -> [ startRef ]
                                       other -> map ref other

        matchesInput numSym        = case symbol numSym of
                                       Any   -> Const True
                                       Sym t -> inStream C.== Const t

        transition   numSym        = matchesInput numSym
                                     C.&&
                                     ( foldl1 ( C.|| )
                                       ( preceding' regexp numSym ) )

        spec         numSym        = [ False ] C.++
                                     -- C.mux reset
                                     -- ( Const False )
                                     ( transition numSym )
        stream       numSym        = ref numSym C..= spec numSym
        streams                    = map stream symbols
    in foldl ( >> ) startStream streams >> outStream'


copilotRegexp inStream regexp outStream reset =
  case runParser start 0 regexp regexp of
    Left  err ->
        error $ "parse error: " ++ show err
    Right regexp ->
        if hasFinitePath regexp then
            error $
            concat [ "The regular expression contains a finite path "
                   , "which is something that will fail to match, "
                   , "since we do not have a distinct end-of-input "
                   , "symbol on infinite streams." ]
        else if hasEpsilon regexp then
                 error $
                 concat [ "The regular expression matches a language "
                        , "that contains epsilon. This cannot be handled "
                        , "on infinite streams, since we do not have "
                        , "a distinct end-of-input symbol."]
             else regexp2CopilotNFA inStream regexp outStream reset


testRegExp' = do { let input  = C.varW8 "input"
                       output = C.varB  "output"
                       reset  = C.varB  "reset"
                 ; input C..= [ 0, 1, 2, 3, 3, 4, 3, 4, 1, 1, 1 ] C.++ input
                 ; reset C..= [ True ] C.++ Const False
                 ; copilotRegexp input "<0><1><4>*<2>(<3><3>*|<4>)*" output reset
                 }

testRegExp = do
  interpret testRegExp' 15 baseOpts
  return ()
