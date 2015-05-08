{-# LANGUAGE FlexibleContexts #-}
-- | 
-- Module: RegExp
-- Description: Regular expression library
-- Copyright: (c) 2011 National Institute of Aerospace / Galois, Inc.
--
-- A regular expression library.
--
-- For examples, see @Examples/RegExpExamples.hs@ in the
-- <https://github.com/leepike/Copilot/tree/master/Examples Copilot repository>.

module Copilot.Library.RegExp ( copilotRegexp, copilotRegexpB ) where

import Text.ParserCombinators.Parsec
  ( optional, (<|>), string, char, between, GenParser, many, choice, CharParser
  , optionMaybe, chainr1, chainr, many1, digit, letter, eof, parse
  , SourceName )
import Data.Int
import Data.Word
import Data.List
import Data.Char
import Data.Maybe

import Control.Monad.State ( evalState, get, modify )

import qualified Copilot.Language as C

-- The symbols in a regular expression, "Any" is any value of type t
-- (matches any symbol, the "point" character in a regular expression).
data Sym t = Any | Sym t
             deriving ( Eq, Ord, Show )

-- A symbol's value can occur multiple times in a regular expression,
-- e.g. "t(tfft)*". A running number "symbolNum" is used to make all
-- symbols in a regular expression unique.
type NumT     = Int
data NumSym t = NumSym { symbolNum :: Maybe NumT
                       , symbol    :: Sym t
                       } deriving ( Eq, Show )

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
lquote, rquote, lparen, rparen,
  star, plus, qmark, point, minus,
  nondigit :: CharParser () Char
lquote = char '<'
rquote = char '>'
lparen = char '('
rparen = char ')'
star   = char '*'
plus   = char '+'
qmark  = char '?'
point  = char '.'
minus  = char '-'

nondigit = char '_' <|> letter

-- A "followedBy" combinator for parsing, parses
-- p, then p' and returns the result of p.
followedBy :: GenParser tok () a
           -> GenParser tok () b
           -> GenParser tok () a
followedBy p p' = p >>= \ r -> p' >> return r


-- Parsing a string p' with prefix p, returning
-- both in order
cPrefix, optCPrefix :: GenParser tok () Char
                    -> GenParser tok () String
                    -> GenParser tok () String
cPrefix p p' = p  >>= \ c -> fmap ( c : ) p'

-- Parsing a string p' with the character p as an
-- optional prefix, return the result with the
-- optional prefix.
optCPrefix p p' = optionMaybe p
                  >>= \ r -> case r of
                               Nothing -> p'
                               Just c  -> fmap ( c : ) p'

-- The ci function ("case insensitive") takes one argument of
-- type string, parses for the string in a case insensitive
-- manner and yields the parsed string (preserving its case).
ci :: String -> GenParser Char () String
ci = mapM ( \ c -> ( char . toLower ) c <|> ( char . toUpper ) c )


-- the parser for regular expressions
regexp  :: ( SymbolParser t ) => GenParser Char () ( RegExp t )
regexp  = chainr1 term opOr

term    :: ( SymbolParser t ) => GenParser Char () ( RegExp t )
term    = chainr factor opConcat REpsilon

factor  :: ( SymbolParser t ) => GenParser Char () ( RegExp t )
factor  = opSuffix factor'

factor' :: ( SymbolParser t ) => GenParser Char () ( RegExp t )
factor' = between lparen rparen regexp
          <|> anySym
          <|> parseSym

-- Parses the "." - point character used to match any symbol.
anySym  :: ( SymbolParser t ) => GenParser Char () ( RegExp t )
anySym  = point >> ( return . RSymbol ) ( NumSym Nothing Any )


class SymbolParser t where
    parseSym :: GenParser Char () ( RegExp t )


instance SymbolParser Bool where
    parseSym = do { truth <- ( ci "t" >> optional ( ci "rue" )
                               >> return True )
                              <|> ( ci "f" >> optional ( ci "alse" )
                                    >> return False )
                              <|> ( string "1" >> return True )
                              <|> ( string "0" >> return False )
                  ; return $ RSymbol ( NumSym Nothing $ Sym truth )
                  }


parseWordSym :: ( Integral t )
                => GenParser Char () ( RegExp t )
parseWordSym = do { num <- between lquote rquote $ many1 digit
                  ; return . RSymbol . NumSym Nothing . Sym
                    $ fromIntegral ( read num :: Integer )
                  }

parseIntSym :: ( Integral t )
                => GenParser Char () ( RegExp t )
parseIntSym = do { num <- between lquote rquote $
                          optCPrefix minus ( many1 digit )
                 ; return . RSymbol . NumSym Nothing . Sym
                   $ fromIntegral ( read num :: Integer )
                 }


type StreamName = String
newtype P = P { getName :: StreamName }
    deriving Eq


parsePSym :: GenParser Char () ( RegExp P )
parsePSym = do { pStream <- between lquote rquote $
                            cPrefix nondigit ( many $ nondigit <|> digit )
               ; return . RSymbol . NumSym Nothing . Sym
                 $ P pStream
               }


instance SymbolParser Word8 where
    parseSym = parseWordSym

instance SymbolParser Word16 where
    parseSym = parseWordSym

instance SymbolParser Word32 where
    parseSym = parseWordSym

instance SymbolParser Word64 where
    parseSym = parseWordSym

instance SymbolParser Int8 where
    parseSym = parseIntSym

instance SymbolParser Int16 where
    parseSym = parseIntSym

instance SymbolParser Int32 where
    parseSym = parseIntSym

instance SymbolParser Int64 where
    parseSym = parseIntSym

instance SymbolParser P where
    parseSym = parsePSym


opOr       :: GenParser Char () ( RegExp t -> RegExp t -> RegExp t )
opOr       = char '|' >> return ROr

opConcat   :: GenParser Char () ( RegExp t -> RegExp t -> RegExp t )
opConcat   = return RConcat

opSuffix   :: GenParser Char () ( RegExp t )
           -> GenParser Char () ( RegExp t )
opSuffix r = do 
  subexp   <- r
  suffixes <- many $ choice [ star, plus, qmark ]
  let transform rexp suffix =
          case suffix of
            '*'   -> RStar   rexp
            '+'   -> RConcat rexp ( RStar rexp )
            '?'   -> ROr     rexp   REpsilon
            other -> C.badUsage ("in Regular Expression library: " ++
                               "unhandled operator: " ++ show other)
  return $ foldl transform subexp suffixes

parser :: ( SymbolParser t )
         => GenParser Char () ( RegExp t )
parser = regexp `followedBy` eof


hasEpsilon                    :: RegExp t -> Bool
hasEpsilon   REpsilon         = True
hasEpsilon ( RSymbol  _     ) = False
hasEpsilon ( ROr      r1 r2 ) = hasEpsilon r1 || hasEpsilon r2
hasEpsilon ( RConcat  r1 r2 ) = hasEpsilon r1 && hasEpsilon r2
hasEpsilon ( RStar    _     ) = True


first                    :: RegExp t -> [ NumSym t ]
first   REpsilon         = []
first ( RSymbol  s     ) = [ s ]
first ( ROr      r1 r2 ) = first r1 ++ first r2
first ( RConcat  r1 r2 ) = first r1 ++ if hasEpsilon r1 then
                                           first r2 else []
first ( RStar    r     ) = first r


reverse'                   :: RegExp t -> RegExp t
reverse' ( ROr     r1 r2 ) = ROr     ( reverse' r1 ) ( reverse' r2 )
reverse' ( RConcat r1 r2 ) = RConcat ( reverse' r2 ) ( reverse' r1 )
reverse' ( RStar   r     ) = RStar   ( reverse' r  )
reverse'   e               = e


last' :: RegExp t -> [ NumSym t ]
last' = first . reverse'


follow                        :: ( Eq t ) =>
                                 RegExp t -> NumSym t -> [ NumSym t ]
follow   REpsilon         _   = []
follow ( RSymbol  _     ) _   = []
follow ( ROr      r1 r2 ) sNr = follow r1 sNr ++ follow r2 sNr
follow ( RConcat  r1 r2 ) sNr = follow r1 sNr ++ follow r2 sNr
                                ++ if sNr `elem` last' r1 then
                                       first r2 else []
follow ( RStar    r     ) sNr = follow r sNr
                                `union` if sNr `elem` last' r then
                                            first r else []


preceding :: ( Eq t ) => RegExp t -> NumSym t -> [ NumSym t ]
preceding = follow . reverse'


hasFinitePath                   :: RegExp t -> Bool
hasFinitePath ( ROr     r1 r2 ) = hasFinitePath r1 || hasFinitePath r2
hasFinitePath ( RConcat _  r2 ) = hasFinitePath r2
hasFinitePath ( RStar   _     ) = False
hasFinitePath   _               = True


getSymbols                   :: RegExp t -> [ NumSym t ]
getSymbols ( RSymbol s     ) = [ s ]
getSymbols ( ROr     r1 r2 ) = getSymbols r1 ++ getSymbols r2
getSymbols ( RConcat r1 r2 ) = getSymbols r1 ++ getSymbols r2
getSymbols ( RStar   r     ) = getSymbols r
getSymbols   _               = []


-- assign each symbol in the regular expression a
-- unique number, counting up from 0
enumSyms   :: RegExp t -> RegExp t
enumSyms rexp = evalState ( enumSyms' rexp ) 0
    where
      enumSyms' ( RSymbol s     ) = do
        num <- get
        modify ( + 1 )
        return $ RSymbol s { symbolNum = Just num }
      enumSyms' ( ROr     r1 r2 ) = do
        r1' <- enumSyms' r1
        r2' <- enumSyms' r2
        return $ ROr r1' r2'
      enumSyms' ( RConcat r1 r2 ) = do
        r1' <- enumSyms' r1
        r2' <- enumSyms' r2
        return $ RConcat r1' r2'
      enumSyms' ( RStar   r     ) = do
        r'  <- enumSyms' r
        return $ RStar   r'
      enumSyms'   other           =
        return other


regexp2CopilotNFA :: ( C.Typed t, Eq t )
                     => C.Stream t -> RegExp t -> C.Stream Bool -> C.Stream Bool
regexp2CopilotNFA inStream rexp reset =
    let symbols                    = getSymbols rexp
        first'                     = first rexp
        start                      = [ True ] C.++ C.false

        preceding'   numSym        = let ps    = preceding rexp numSym
                                         s     = if numSym `elem` first' then
                                                   [ start ] else []
                                     in s ++ [ streams !! i
                                             | i <- map ( fromJust . symbolNum ) ps ]

        matchesInput numSym        = case symbol numSym of
                                       Any   -> C.true
                                       Sym t -> inStream C.== C.constant t

        transitions  numSym ps     = matchesInput numSym
                                     C.&& ( foldl ( C.|| ) C.false ps )

        stream       numSym        = let ps    = preceding' numSym
                                         init_ = C.constant $ numSym `elem` first'
                                     in C.mux reset
                                        ( [ False ] C.++ matchesInput numSym C.&& init_ )
                                        ( [ False ] C.++ transitions  numSym ps )

        streams                    = map stream symbols

        outStream                  = foldl ( C.|| ) start streams

    in outStream


copilotRegexp :: ( C.Typed t, SymbolParser t, Eq t )
                 => C.Stream t -> SourceName -> C.Stream Bool -> C.Stream Bool
copilotRegexp inStream rexp reset =
  case parse parser rexp rexp of
    Left  err -> C.badUsage ("parsing regular exp: " ++ show err)
    Right rexp' -> let nrexp = enumSyms rexp' in
        if hasFinitePath nrexp then
            C.badUsage $
            concat [ "The regular expression contains a finite path "
                   , "which is something that will fail to match "
                   , "since we do not have a distinct end-of-input "
                   , "symbol on infinite streams." ]
        else if hasEpsilon nrexp then
                 C.badUsage $
                 concat [ "The regular expression matches a language "
                        , "that contains epsilon. This cannot be handled "
                        , "on infinite streams, since we do not have "
                        , "a distinct end-of-input symbol." ]
             else regexp2CopilotNFA inStream nrexp reset


regexp2CopilotNFAB :: RegExp P -> [ ( StreamName, C.Stream Bool ) ]
                      -> C.Stream Bool -> C.Stream Bool
regexp2CopilotNFAB rexp propositions reset =
    let symbols                    = getSymbols rexp
        first'                     = first rexp
        start                      = [ True ] C.++ C.false

        preceding'   numSym        = let ps    = preceding rexp numSym
                                         s     = if numSym `elem` first' then
                                                   [ start ] else []
                                     in s ++ [ streams !! i
                                             | i <- map ( fromJust . symbolNum ) ps ]

        lookup' a l = case lookup a l of
                        Nothing -> C.badUsage ("boolean stream "
                                             ++ a
                                             ++ " is not defined")
                        Just s  -> s

        matchesInput numSym        = case symbol numSym of
                                       Any   -> C.true
                                       Sym t -> lookup' ( getName t ) propositions

        transitions  numSym ps     = matchesInput numSym
                                     C.&& ( foldl ( C.|| ) C.false ps )

        stream       numSym        = let ps    = preceding' numSym
                                         init_ = C.constant $ numSym `elem` first'
                                     in C.mux reset
                                        ( [ False ] C.++ matchesInput numSym C.&& init_ )
                                        ( [ False ] C.++ transitions  numSym ps )

        streams                    = map stream symbols

        outStream                  = foldl ( C.|| ) start streams

    in outStream


copilotRegexpB :: SourceName -> [ ( StreamName, C.Stream Bool ) ]
                  -> C.Stream Bool -> C.Stream Bool
copilotRegexpB rexp propositions reset =
  case parse parser rexp rexp of
    Left  err -> C.badUsage ("parsing regular exp: " ++ show err)
    Right rexp' -> let nrexp = enumSyms rexp' in
        if hasFinitePath nrexp then
            C.badUsage $
            concat [ "The regular expression contains a finite path "
                   , "which is something that will fail to match "
                   , "since we do not have a distinct end-of-input "
                   , "symbol on infinite streams." ]
        else if hasEpsilon nrexp then
                 C.badUsage $
                 concat [ "The regular expression matches a language "
                        , "that contains epsilon. This cannot be handled "
                        , "on infinite streams, since we do not have "
                        , "a distinct end-of-input symbol." ]
             else regexp2CopilotNFAB nrexp propositions reset
