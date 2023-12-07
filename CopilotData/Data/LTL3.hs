{-# LANGUAGE RankNTypes #-}
module CopilotData.Data.LTL3 where
import           Data.List           ( lookup, nub, sort )
import           Data.Maybe          ( fromJust, maybeToList )
import           Debug.Trace
import           Language.Copilot    hiding ( not )
import           Prelude             hiding ( mod, (&&), (++), (/=), (<), (==),
                                       (>) )
import qualified Prelude             as P
import           System.IO
import           System.IO.Unsafe
import           Text.Pretty.Simple  (pPrint)

import CopilotData.Data.Buchi
import qualified CopilotData.Data.DFA as DFA
import CopilotData.Data.FSM
import CopilotData.Data.LTL
import CopilotData.Data.NFA
import CopilotData.Data.State

-- * Three valued logic
type Bool3 = Word8

true3 :: Bool3
true3 = 1

unknown3:: Bool3
unknown3 = 2

false3 :: Bool3
false3 = 3

prettyDebug prefix a = unsafePerformIO $ do
  print prefix
  pPrint a
  hFlush stdout

-- Given a formula phi, monitor is:
monitor :: [(Int, Stream Bool)] -> LTLFormula -> Stream Bool3
monitor streams psi = fsmToMonitor streams $ minimize $ prod
  where
    prod  = -- unsafePerformIO (pPrint prod') `seq`
            prod'
    prod' = productDFA psi

fsmToMonitor :: [(Int, Stream Bool)]
             -> FSM [Int] ([LTLFormula], [LTLFormula]) Bool3
             -> Stream Bool3
fsmToMonitor streams fsm =
  stateMachineGF $ toStateMachine streams fsm

toStateMachine :: [(Int, Stream Bool)]
               -> FSM [Int] ([LTLFormula], [LTLFormula]) Bool3
               -> StateMachineGF
toStateMachine inputs fsm =
    prettyDebug "Graph:" g `seq` g
  where
    g           = (initial, 255, false, transitions, 255, outputMapping)
    alphabet    = fsmAlphabet fsm
    states      = zip (fsmStates fsm) [0..]
    initial     = myFromJust $ lookup (fsmInitialState fsm) states
    transitions = [ (sn, e, sn')

                  | ((s, input), s') <- fsmTransitions fsm

                  , let sn :: Word8
                        sn = myFromJust $ lookup s states

                  , let sn' :: Word8
                        sn' = myFromJust $ lookup s' states

                  , let u :: [Int]
                        u = input

                  , let e = conforms inputs input

                  ]

    outputMapping = [(sn, on) | (s, sn) <- states
                              , on <- map snd $ filter ((s P.==).fst) (fsmOutputFunction fsm)
                              ]

    conforms :: [(Int, Stream Bool)] -> [Int] -> Stream Bool
    conforms inputStreams inputs =
        streamAll $ map snd $ filter (\(k, v) -> k `elem` inputs) inputStreams
      where
        streamAll []     = true
        streamAll (x:xs) = x && streamAll xs

productDFA :: LTLFormula -> FSM [Int] ([LTLFormula], [LTLFormula]) Bool3
productDFA psi = r
  where
    r =  FSM sigma qbar q0bar deltabar [true3, false3, unknown3] lambdabar

    sigma :: [[Int]]
    sigma = DFA.alphabet a1

    qbar  = [(s1, s2) | s1 <- DFA.states a1, s2 <- DFA.states a2 ]
    q0bar = (DFA.initialState a1, DFA.initialState a2)
    deltabar = [ ( ((q, q'), a)
                 , (x, y)
                 )
               | (q, q') <- qbar
               , a <- DFA.alphabet a1
               , x <- map snd $ filter (((q,  a) P.==).fst) $ DFA.transitions a1
               , y <- map snd $ filter (((q', a) P.==).fst) $ DFA.transitions a2
               ]

    -- I think the bug is here
    lambdabar = [ ((q, q'), false3)
                | (q, q') <- qbar
                , q `notElem` DFA.finalStates a1
                ]

                P.++

                [ ((q, q'), true3)
                | (q, q') <- qbar
                , q' `notElem` DFA.finalStates a2
                ]

                P.++

                [ ((q, q'), unknown3)
                | (q, q') <- qbar
                , q' `elem` DFA.finalStates a1 P.&& q' `elem` DFA.finalStates a2
                ]

    a1 :: DFA.DFA [Int] [LTLFormula]
    a1 = dfaFromStream psi

    a2 :: DFA.DFA [Int] [LTLFormula]
    a2 = dfaFromStream (LTLNot psi)

dfaFromStream :: LTLFormula -> DFA.DFA [Int] [LTLFormula]
dfaFromStream l = -- (prettyDebug "DFA from stream: " (r, n)) `seq` r
                  r
  where
    r = DFA.powerset n
    n = nfaFromStream l

nfaFromStream :: LTLFormula -> NFA [Int] LTLFormula
nfaFromStream psi = r -- (prettyDebug "NFA: " r) `seq` r
  where
    r  = NFA a ss s0 d f
    a  = nbaAlphabet nba
    ss = nbaState nba
    s0 = nbaInitialState nba
    d  = nbaTransitions nba
    f  = [ q | q <- nbaFinalStates $ nba, curlyF psi q ]

    curlyF psi s = isLanguageEmpty (nba { nbaInitialState = s })

    nba = nbaFromStream psi

nbaFromStream :: LTLFormula -> NBA [Int] LTLFormula
nbaFromStream formula = -- (prettyDebug "NBA: " r) `seq` r
                        r
                        -- r
  where
    r = NBA
          { nbaAlphabet     = alphabet
          , nbaInitialState = formula
          , nbaState        = states
          , nbaTransitions  = trans'
          , nbaFinalStates  = [ s | s@(LTLNot (x1 `LTLUntil` x2)) <- states ]
          }

    alphabet = sublists $ allProps formula

    states = sort $ nub $ map reduceNots $ subformulas formula P.++ subformulas (LTLNot formula)
                                           P.++ [LTLTrue, LTLNot LTLTrue]
    -- states' = sort $ nub $ states P.++ filter (\x -> x == LTLTrue \/ x == LTLNot LTLTrue) (map snd trans')

    trans' :: [((LTLFormula, [Int]), [LTLFormula])]
    trans' = [ ((x, a), s) | x <- states, a <- alphabet, let s = trans (x, a) ]

    trans :: (LTLFormula, [Int]) -> [LTLFormula]
    trans (LTLProp i, a)    = [ if i `elem` a then LTLTrue else ltlFalse ]
    trans (LTLTrue, _)      = []
    trans (LTLNot f, a)     = map complement' (trans (f, a))
    trans (LTLAnd p1 p2, a) = [ LTLAnd v1 v2 | v1 <- trans (p1, a),
                                               v2 <- trans (p2, a)]
    trans (LTLUntil p1 p2, a) = [ ltlOr v1 v3 | v1 <- trans (p1, a)
                                              , v2 <- trans (p2, a)
                                              , let v3 = LTLAnd v2 (LTLUntil p1 p2)
                                ]
    trans (LTLNext p1, _) = [ p1 ]

reduceNots :: LTLFormula -> LTLFormula
reduceNots (LTLNot (LTLNot x)) = reduceNots x
reduceNots (LTLNot   f)        = LTLNot   (reduceNots f)
reduceNots (LTLAnd   f1 f2)    = LTLAnd   (reduceNots f1) (reduceNots f2)
reduceNots (LTLUntil f1 f2)    = LTLUntil (reduceNots f1) (reduceNots f2)
reduceNots (LTLNext  f)        = LTLNext  (reduceNots f)
reduceNots f                   = f

complement' :: LTLFormula -> LTLFormula
complement' = reduceNots . complement'' . reduceNots

complement'' LTLTrue                                 = ltlFalse
complement'' (LTLAnd x1 x2)                          = ltlOr (complement'' x1) (complement'' x2)
complement'' (LTLNot (LTLNot x1 `LTLAnd` LTLNot x2)) = LTLAnd (complement'' x1) (complement'' x2)
complement'' (LTLNot (x1 `LTLAnd` x2))               = LTLAnd (complement'' (LTLNot x1)) (complement'' (LTLNot x2))
complement'' (LTLNot (LTLNot x1))                    = LTLNot (complement'' x1)
complement'' x1                                      = LTLNot x1

-- * Auxiliary

-- | List all sublists of a list.
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = remainder P.++ map (x:) remainder
  where
    remainder = sublists xs

testNBA = nbaFromStream (LTLProp 55)
testNFA = nfaFromStream (LTLProp 55)

-- -- myTrans' :: [((LTLFormula, [Int]), [LTLFormula])]
-- myTrans' states alphabet = [ ((x, a), s) | x <- states, a <- alphabet, let s = myTrans states alphabet (x, a) ]
--
-- -- myTrans :: (LTLFormula, [Int]) -> [LTLFormula]
-- myTrans states alphabet (LTLProp i, a)    = [ if i `elem` a then LTLTrue else ltlFalse ]
-- myTrans states alphabet (LTLTrue, _)      = []
-- myTrans states alphabet (LTLNot f, a)     = let e = (myTrans states alphabet (f, a))
--                                                 e' = map complement' e
--                                             in trace ("This: " P.++ show (e, e')) $ e'
-- myTrans states alphabet (LTLAnd p1 p2, a) = [ LTLAnd v1 v2 | v1 <- myTrans states alphabet (p1, a),
--                                                              v2 <- myTrans states alphabet (p2, a)]
-- myTrans states alphabet (LTLUntil p1 p2, a) = [ ltlOr v1 v3 | v1 <- myTrans states alphabet (p1, a)
--                                                             , v2 <- myTrans states alphabet (p2, a)
--                                                             , let v3 = LTLAnd v2 (LTLUntil p1 p2)
--                                                             ]
-- myTrans states alphabet (LTLNext p1, _) = [ p1 ]
--
--
myFromJust Nothing = error "LTL3FromJust"
myFromJust x = fromJust x
