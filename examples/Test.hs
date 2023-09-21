{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE GADTs #-}

-- import Debug.Trace

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.QuickCheck.Monadic

import Prelude hiding (True, False)
import qualified Language.Copilot
import qualified Copilot.Interpret
import Copilot.PrettyPrint

import Data.List
import Data.String
import Data.Char
import System.IO

import qualified Mltl

data Formula where
  -- labeled variables/streams
  Var :: Int -> Formula

  -- constants (sometimes useful in LTL formulae)
  True :: Formula
  False :: Formula
  PresumablyTrue :: Formula  -- unsure if this is needed
  PresumablyFalse :: Formula -- unsure if this is needed

  -- standard logical operators
  And :: Formula -> Formula -> Formula
  Or :: Formula -> Formula -> Formula
  Implies :: Formula -> Formula -> Formula
  Not :: Formula -> Formula

  -- PTLTL operators, unbounded
  AlwaysBeen :: Formula -> Formula     -- past tense globally
  Previous :: Formula -> Formula         -- is it true in the previous time step
  Ever :: Formula -> Formula             -- has it ever been true in any previous time step
  Since :: Formula -> Formula -> Formula -- has the first stream held since the second stream has been true

  -- FTLTL operators, unbounded
  Globally :: Formula -> Formula         -- globally/box
  Next :: Formula -> Formula             -- is it true in the next time step
  Finally :: Formula -> Formula          -- will it be true in any future time step
  Until :: Formula -> Formula -> Formula -- has the first stream held until the second stream has been true

  -- MLTL operators, bounded
  GloballyWithin :: Int -> Int -> Formula -> Formula
  FinallyWithin :: Int -> Int -> Formula -> Formula
  UntilWithin :: Int -> Int -> Formula -> Formula -> Formula

  deriving (Eq, Ord, Read)

instance Show Formula where
  -- questions: what is zeta? should I implement equiv, xor, etc?
  show (Var n) = "ATOM(state_fun(" ++ show n ++ "))"
  show True  = "TRUE"
  show False  = "FALSE"
  show PresumablyTrue  = "TRUE"
  show PresumablyFalse  = "FALSE"

  show (And f1 f2) = "AND_MTL(" ++ show f1 ++ "," ++ show f2 ++ ")"
  show (Or f1 f2) = "OR_MTL(" ++ show f1 ++ "," ++ show f2 ++ ")"
  show (Implies f1 f2) = "IMPLIES_MTL(" ++ show f1 ++ "," ++ show f2 ++ ")"
  show (Not f1) = "NOT_MTL(" ++ show f1 ++ ")"

  show (AlwaysBeen f1) = "HISTORICALLY(" ++ show f1 ++ ")"
  show (Previous f1) = "PREVIOUS(" ++ show f1 ++ ")"
  show (Ever f1) = "ONCE(" ++ show f1 ++ ")"
  show (Since f1 f2) = "SINCE(" ++ show f1 ++ "," ++ show f2 ++ ")"

  show (Globally f1) = "GLOBALLY(" ++ show f1 ++ ")"
  show (Next f1) = "NEXT(" ++ show f1 ++ ")"
  show (Finally f1) = "FINALLY(" ++ show f1 ++ ")"
  show (Until f1 f2) = "UNTIL(" ++ show f1 ++ "," ++ show f2 ++ ")"
   
  show (GloballyWithin m n f1) = "GLOBALLY_BD(" ++ show f1 ++ ",[|" ++ show m ++ "," ++ show n ++ "|])"
  show (FinallyWithin m n f1) = "FINALLY_BD(" ++ show f1 ++ ",[|" ++ show m ++ "," ++ show n ++ "|])"
  show (UntilWithin m n f1 f2) = "UNTIL_BD(" ++ show f1 ++ "," ++ show f2 ++ ",[|" ++ show m ++ "," ++ show n ++ "|])"


-- used for generating streams
-- might need to switch to listOf (otherwise terms may explode, depending on how lazy the Copilot interpreter is)
genInputs :: Int -> Gen [Language.Copilot.Int8]
genInputs size = fmap (Prelude.take size) (infiniteListOf (elements [Mltl.fc, Mltl.tc]))

genInputsP :: Int -> Gen [Language.Copilot.Int8]
genInputsP size = fmap (Prelude.take size) (infiniteListOf (elements [Mltl.fc, Mltl.pfc, Mltl.ptc, Mltl.tc]))

-- QuickCheck generators
genConstants :: Gen Formula
genConstants = elements [True, False]

genConstantsP :: Gen Formula
genConstantsP = elements [True, False, PresumablyTrue, PresumablyFalse]

genUnary :: Gen (Formula -> Formula)
genUnary = (elements [Not, AlwaysBeen, Previous, Ever, Globally, Next, Finally]) -- <*> f

genBinary :: Gen (Formula -> Formula -> Formula)
genBinary = elements [And, Or, Implies, Since, Until]

genVars :: Int -> [Formula]
genVars n = map (\x -> Var x) [0..n]

-- generate formulae with up to n variables (nb: I removed genConstants from this list)
genFormula :: Int -> Gen Formula
genFormula n = oneof [vars >>= elements, genUnary <*> genFormula n, (genBinary <*> genFormula n) <*> genFormula n]
  where
    vars = shuffle (genVars n)

genBoundedFormula :: Int -> Int -> Int -> Gen Formula -> Gen Formula
genBoundedFormula lowerBound upperBound n generator = do 
  lower <- chooseBoundedIntegral (lowerBound, upperBound)
  upper <- chooseBoundedIntegral (lower, upperBound)
  f1 <- generator
  f2 <- generator
  elements [
      (GloballyWithin lower upper f1),
      (FinallyWithin lower upper f1),
      (UntilWithin lower upper f1 f2)
    ]

genFormulaWeightedBound :: Int -> Int -> Int ->  Gen Formula
genFormulaWeightedBound upperBound lowerBound n = genFormulaWeightedBoundHelp upperBound lowerBound n 4

genFormulaWeightedBoundHelp upperBound lowerBound n iter = frequency [
    (2 * iter, vars >>= elements), 
    -- (1 * iter, genConstants), 
    (3, genUnary <*> self), 
    (3, (genBinary <*> self) <*> self),
    (2, genBoundedFormula upperBound lowerBound n self)
  ] 
  where 
    self = genFormulaWeightedBoundHelp upperBound lowerBound n (iter+1)
    vars = shuffle (genVars n)

toCopilot :: Formula -> [(Int, Language.Copilot.Stream Language.Copilot.Int8)] -> Language.Copilot.Stream Language.Copilot.Int8
toCopilot (Var n) varMap = case lookup n varMap of
  Just x -> x
  Nothing -> error "Invalid variable lookup"
toCopilot (True) varMap = Mltl.t
toCopilot (False) varMap = Mltl.f
toCopilot (PresumablyTrue) varMap = Mltl.pt
toCopilot (PresumablyFalse) varMap = Mltl.pf

toCopilot (And f1 f2) varMap = Mltl.and' (toCopilot f1 varMap) (toCopilot f2 varMap)
toCopilot (Or f1 f2) varMap = Mltl.or' (toCopilot f1 varMap) (toCopilot f2 varMap)
toCopilot (Implies f1 f2) varMap = Mltl.implies' (toCopilot f1 varMap) (toCopilot f2 varMap)
toCopilot (Not f1) varMap = Mltl.not' (toCopilot f1 varMap) 

-- default to mine when possible
toCopilot (AlwaysBeen f1) varMap = Mltl.alwaysBeenMine (toCopilot f1 varMap) 
toCopilot (Previous f1) varMap = Mltl.previous (toCopilot f1 varMap) 
toCopilot (Ever f1) varMap = Mltl.everMine (toCopilot f1 varMap) 
toCopilot (Since f1 f2) varMap = Mltl.sinceMine (toCopilot f1 varMap) (toCopilot f2 varMap)

toCopilot (Globally f1) varMap = Mltl.globally' (toCopilot f1 varMap) 
toCopilot (Next f1) varMap = Mltl.eNext (toCopilot f1 varMap) 
toCopilot (Finally f1) varMap = Mltl.finally (toCopilot f1 varMap) 
toCopilot (Until f1 f2) varMap = Mltl.strongUntil' (toCopilot f1 varMap) (toCopilot f2 varMap)

toCopilot (GloballyWithin m n f1) varMap = Mltl.globallyWithin m n (toCopilot f1 varMap)
toCopilot (FinallyWithin m n f1) varMap = Mltl.finallyWithin m n (toCopilot f1 varMap)
toCopilot (UntilWithin m n f1 f2) varMap = Mltl.strongUntilWithin m n (toCopilot f1 varMap) (toCopilot f2 varMap)



formulaSpec :: Formula -> [(Int, [Language.Copilot.Int8])] -> Language.Copilot.Spec
formulaSpec formula inputs = Language.Copilot.observer "output" output
  where
    output = toCopilot formula inputStreams
    inputStreams =
      map (\(k, v) -> (k, Language.Copilot.extern ("x" ++ show k) (Just v))) inputs

data Config = Config
    { lowerBound    :: Int -- the lowest number that a bounded temporal operator will have
    , upperBound    :: Int -- the highest number
    , varCount      :: Int -- the max number of vars in the random LTL formula
    , inputSize     :: Int -- the size of the input stream that will be randomly generated
    }

defaultConfig = Config 
    { lowerBound = 0
    , upperBound = 10
    , varCount = 2
    , inputSize = 20
    }

type Seed = Int

randomFormula :: Config -> QCGen -> Formula
randomFormula config rng = Test.QuickCheck.Gen.unGen gen rng 0 -- this constant is arbitrary
  where
    gen :: Gen Formula
    gen = genFormulaWeightedBound (lowerBound config) (upperBound config) (varCount config) 

printFormula :: Formula -> IO ()
printFormula f = putStrLn $ (show f) ++ " % MTL_FORMULA"

randomInput :: Config -> QCGen -> [Language.Copilot.Int8]
randomInput config rng = Test.QuickCheck.Gen.unGen gen rng 0 -- this constant is arbitrary
  where
    gen :: Gen [Language.Copilot.Int8]
    gen = genInputs (inputSize config)

-- printInputs :: [(Int, [Language.Copilot.Int8])] -> IO ()
-- printInputs inputs = mapM_ putStrLn (map show inputs))

printInputs :: [(Int, [Language.Copilot.Int8])] -> IO ()
printInputs inputs = putStrLn $ "(:" ++ intercalate ", " (map prettyPrint array) ++ ":)"
  where
    array = (transpose (map snd inputs))
    prettyPrint :: [Language.Copilot.Int8] -> String
    prettyPrint l = "(:" ++ intercalate "," (map printTruth l) ++ ":)"

printTruth :: Language.Copilot.Int8 -> String
printTruth 0 = "FALSE"
-- printTruth 1 = "PRESUMABLY_FALSE"
printTruth 1 = "FALSE"
-- printTruth 2 = "PRESUMABLY_TRUE"
printTruth 2 = "TRUE"
printTruth 3 = "TRUE"
printTruth n = error "undefined when trying to showTruth, encountered " ++ show n
-- printInputs inputs = mapM_ putStrLn (map show inputs)

parseOutput :: String -> [Language.Copilot.Int8]
parseOutput output = map (read . strip) (drop 1 (lines output))
  where
    strip x = filter (not . isSpace) x

-- parses and pretty-prints output
transformOutput :: String -> String
transformOutput output = "(:" ++ intercalate "," (map printTruth (parseOutput output)) ++ ":)"

evaluate :: Formula -> [(Int, [Language.Copilot.Int8])] -> IO String
evaluate formula inputs = do
    reifiedSpec <- Language.Copilot.reify spec
    -- putStrLn $ prettyPrint reifiedSpec -- for debugging
    return (numSamples `seq` Copilot.Interpret.interpret Copilot.Interpret.Table numSamples reifiedSpec)
    -- Language.Copilot.interpret (fromIntegral numSamples) spec
  where
    numSamples :: Int
    numSamples = maximum $ map length $ map snd inputs
    spec = formulaSpec formula inputs

runTest :: Config -> Seed -> IO ()
runTest config seed = do
  -- putStrLn $ show inputs
  putStrLn "% Inputs"
  putStrLn $ show ((inputSize config) - 1) ++ " % last timestep"

  hFlush stdout

  printFormula formula
  printInputs inputs

  hFlush stdout

  putStrLn "% Expected value"
  output <- evaluate formula inputs
  -- putStrLn $ output
  putStrLn $ transformOutput output

  hFlush stdout
  where
    rng :: QCGen
    rng = mkQCGen seed
    formula = randomFormula config (integerVariant 0 rng)
    -- formula = Var 1 -- for debugging
    inputs = map (\n -> (fromIntegral n, randomInput config (integerVariant n rng))) [0..(fromIntegral (varCount config))]

runTests :: Int -> IO()
runTests n = do
  mapM_ (\x -> runTest defaultConfig x) [0..n]

-- main = runTests 2
main = runTests 100000

-- some properties to test
prop_CanGen seed = randomFormula defaultConfig (mkQCGen seed) == randomFormula defaultConfig (mkQCGen seed)

prop_FormulaDeterministic seed = monadicIO $ do
  f1 <- run $ printFormula $ randomFormula defaultConfig (mkQCGen seed)
  f2 <- run $ printFormula $ randomFormula defaultConfig (mkQCGen seed)
  assert (f1 == f2)

prop_InputDeterministic seed = monadicIO $ do
  i1 <- run $ printInputs [(0, randomInput defaultConfig (mkQCGen seed))]
  i2 <- run $ printInputs [(0, randomInput defaultConfig (mkQCGen seed))]
  assert (i1 == i2)

{-
prop_DoubleNegationElim seed varCount = monadicIO $ do
  undefined
  where
    rng :: QCGen
    rng = mkQCGen seed
-}
