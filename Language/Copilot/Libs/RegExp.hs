module Language.Copilot.Libs.RegExp ( copilotRegexp, copilotRegexpB ) where

import Text.ParserCombinators.Parsec
--  ( optional, (<|>), string, char, between, GenParser, many, choice, CharParser
--  , optionMaybe, chainr1, chainr, many1, digit, letter, alphaNum, eof, parse
--  , mappend, SourceName)
import Data.Int
import Data.Word
import Data.List
import Data.Char

import Data.Maybe
import Control.Monad.State (evalState, get, modify)

import Language.Copilot.Core
import qualified Language.Copilot.Language as C


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
-- e.g. "t(tfft)*". A running number "symbolNum" is used to make all
-- symbols in a regular expression unique.
data NumSym t = NumSym { symbolNum :: Maybe Int
                       , symbol    :: Sym t
                       } deriving ( Eq )

-- A show instance for the numbered symbol type, converts a numbered
-- symbol type into a valid C identifier string to be used as a
-- Copilot variable name
instance Show t => Show ( NumSym t ) where
    show s = "rsym_"
             ++ ( replace '-' '_' . show . symbol ) s
             ++ "_"
             ++ show ( fromJust $ symbolNum s )
        where replace c1 c2 = map ( \ c -> if c == c1 then c2 else c )

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

newtype VarName = VarName { getVarName :: Var }
    deriving Eq

instance Show VarName where
    show s = getVarName s

parseVarSym :: GenParser Char () ( RegExp VarName )
parseVarSym = do { varName <- between lquote rquote $
                              cPrefix nondigit ( many $ nondigit <|> digit )
                 ; return . RSymbol . NumSym Nothing . Sym
                   $ VarName varName
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

instance SymbolParser VarName where
    parseSym = parseVarSym


opOr       :: GenParser Char () ( RegExp t -> RegExp t -> RegExp t )
opOr       = char '|' >> return ROr

opConcat   :: GenParser Char () ( RegExp t -> RegExp t -> RegExp t )
opConcat   = return RConcat

opSuffix   :: GenParser Char () ( RegExp t )
           -> GenParser Char () ( RegExp t )
opSuffix r = do { subexp   <- r
                ; suffixes <- many $ choice [ star, plus, qmark ]
                ; let transform rexp suffix =
                          case suffix of
                            '*'   -> RStar   rexp
                            '+'   -> RConcat rexp ( RStar rexp )
                            '?'   -> ROr     rexp   REpsilon
                            other -> error
                                     $ "unhandled operator: " ++ show other
                  in return $ foldl transform subexp suffixes
                }

start :: ( SymbolParser t )
         => GenParser Char () ( RegExp t )
start = regexp `followedBy` eof


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


regexp2CopilotNFA :: ( Streamable t, C.EqE t )
                  => Spec t -> RegExp t -> Spec Bool -> Spec Bool -> Streams
regexp2CopilotNFA inStream rexp outStream reset =
    let symbols                    = getSymbols rexp
        ref                        = C.var . show
        startRef                   = C.var "start"
        startStream                = startRef  C..= reset
        outStream'                 = outStream C..=
                                     foldl ( C.|| ) reset ( map ref symbols )

        preceding'   rexp'  numSym = case preceding rexp' numSym of
                                       []    -> [ startRef ]
                                       other -> map ref other

        matchesInput numSym        = case symbol numSym of
                                       Start -> error "start matched"
                                       Any   -> C.true
                                       Sym t -> inStream C.== C.const t

        transition   numSym        = matchesInput numSym
                                     C.&&
                                     foldl1 ( C.|| )
                                       ( preceding' rexp numSym )

        spec         numSym        = [ False ] C.++
                                     C.mux ( C.drop 1 reset )
                                     C.false
                                     ( transition numSym )

        stream       numSym        = ref numSym C..= spec numSym
        streams                    = map stream symbols
    in foldl ( >> ) startStream streams >> outStream'


copilotRegexp :: (Streamable t, SymbolParser t, C.EqE t)
              => Spec t -> SourceName -> Spec Bool -> Spec Bool -> Streams
copilotRegexp inStream rexp outStream reset =
  case parse start rexp rexp of
    Left  err ->
        error $ "parse error: " ++ show err
    Right rexp' -> let nrexp = enumSyms rexp' in
        if hasFinitePath nrexp then
            error $
            concat [ "The regular expression contains a finite path "
                   , "which is something that will fail to match "
                   , "since we do not have a distinct end-of-input "
                   , "symbol on infinite streams." ]
        else if hasEpsilon nrexp then
                 error $
                 concat [ "The regular expression matches a language "
                        , "that contains epsilon. This cannot be handled "
                        , "on infinite streams, since we do not have "
                        , "a distinct end-of-input symbol."]
             else regexp2CopilotNFA inStream nrexp outStream reset


regexp2CopilotNFAB :: RegExp VarName -> Spec Bool -> Spec Bool -> Streams
regexp2CopilotNFAB rexp outStream reset =
    let symbols                    = getSymbols rexp
        ref                        = C.var . show
        startRef                   = C.var "start"
        startStream                = startRef  C..= reset
        outStream'                 = outStream C..=
                                     foldl ( C.|| ) reset ( map ref symbols )

        preceding'   rexp'  numSym = case preceding rexp' numSym of
                                       []            -> [ startRef ]
                                       other         -> map ref other

        matchesInput numSym        = case symbol numSym of
                                       Start             -> error
                                                            "start matched"
                                       Any               -> C.true
                                       Sym ( VarName t ) -> C.var t

        transition   numSym        = matchesInput numSym
                                     C.&&
                                     foldl1 ( C.|| )
                                       ( preceding' rexp numSym )

        spec         numSym        = [ False ] C.++
                                     C.mux ( C.drop 1 reset )
                                     C.false
                                     ( transition numSym )

        stream       numSym        = ref numSym C..= spec numSym
        streams                    = map stream symbols
    in foldl ( >> ) startStream streams >> outStream'


copilotRegexpB :: SourceName -> Spec Bool -> Spec Bool -> Streams
copilotRegexpB rexp outStream reset =
  case parse start rexp rexp of
    Left  err ->
        error $ "parse error: " ++ show err
    Right rexp' -> let nrexp = enumSyms rexp' in
        if hasFinitePath nrexp then
            error $
            concat [ "The regular expression contains a finite path "
                   , "which is something that will fail to match "
                   , "since we do not have a distinct end-of-input "
                   , "symbol on infinite streams." ]
        else if hasEpsilon nrexp then
                 error $
                 concat [ "The regular expression matches a language "
                        , "that contains epsilon. This cannot be handled "
                        , "on infinite streams, since we do not have "
                        , "a distinct end-of-input symbol."]
             else regexp2CopilotNFAB nrexp outStream reset
