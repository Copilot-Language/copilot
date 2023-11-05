{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Test copilot-c99:Copilot.Compile.C99.
module Test.Copilot.Compile.C99
    ( tests )
  where

-- External imports
import Control.Arrow                        ((&&&))
import Control.Exception                    (IOException, catch)
import Control.Monad                        (when)
import Data.Bits                            (Bits, complement)
import Data.List                            (intercalate)
import Data.Type.Equality                   (testEquality)
import Data.Typeable                        (Proxy (..), (:~:) (Refl))
import GHC.TypeLits                         (KnownNat, natVal)
import System.Directory                     (doesFileExist,
                                             getTemporaryDirectory,
                                             removeDirectory, removeFile,
                                             setCurrentDirectory)
import System.IO                            (hPutStrLn, stderr)
import System.Posix.Temp                    (mkdtemp)
import System.Process                       (callProcess, readProcess)
import System.Random                        (Random)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Arbitrary, Gen, Property,
                                             arbitrary, choose, elements,
                                             forAll, forAllBlind, frequency,
                                             getPositive, ioProperty, oneof,
                                             vectorOf, (.&&.))
import Test.QuickCheck.Gen                  (chooseAny, chooseBoundedIntegral)

-- External imports: Copilot
import Copilot.Core            hiding (Property)
import Copilot.Core.Type.Array (array)

-- External imports: Modules being tested
import Copilot.Compile.C99 (cSettingsOutputDirectory, compile, compileWith,
                            mkDefaultCSettings)

-- * Constants

-- | All unit tests for copilot-core:Copilot.Core.Type.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Compile.C99"
    [ testProperty "Compile specification"               testCompile
    , testProperty "Compile specification in custom dir" testCompileCustomDir
    , testProperty "Run specification"                   testRun
    , testProperty "Run and compare results"             testRunCompare
    ]

-- * Individual tests

-- | Test compiling a spec.
testCompile :: Property
testCompile = ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "copilot_test_"
    setCurrentDirectory testDir

    compile "copilot_test" spec
    r <- compileC "copilot_test"

    -- Remove file produced by GCC
    removeFile "copilot_test.o"

    -- Remove files produced by Copilot
    removeFile "copilot_test.c"
    removeFile "copilot_test.h"
    removeFile "copilot_test_types.h"

    setCurrentDirectory tmpDir
    removeDirectory testDir

    return r

  where

    spec = Spec streams observers triggers properties

    streams    = [ Stream 0 [1] (Const Int8 1) Int8]
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "func"

    guard = Const Bool True

    args = []

-- | Test compiling a spec in a custom directory.
testCompileCustomDir :: Property
testCompileCustomDir = ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "copilot_test_"

    compileWith (mkDefaultCSettings { cSettingsOutputDirectory = testDir })
                "copilot_test"
                spec

    setCurrentDirectory testDir
    r <- compileC "copilot_test"

    -- Remove file produced by GCC
    removeFile "copilot_test.o"

    -- Remove files produced by Copilot
    removeFile "copilot_test.c"
    removeFile "copilot_test.h"
    removeFile "copilot_test_types.h"

    setCurrentDirectory tmpDir
    removeDirectory testDir

    return r

  where

    spec = Spec streams observers triggers properties

    streams    = [ Stream 0 [1] (Const Int8 1) Int8]
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "nop"

    guard = Const Bool True

    args = []

-- | Test compiling a spec and running the resulting program.
--
-- The actual behavior is ignored.
testRun :: Property
testRun = ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "copilot_test_"
    setCurrentDirectory testDir

    compile "copilot_test" spec
    r <- compileC "copilot_test"

    let cProgram = unlines
          [ "#include \"copilot_test.h\""
          , ""
          , "void nop () {"
          , "}"
          , ""
          , "void main () {"
          , "  step();"
          , "}"
          ]

    writeFile "main.c" cProgram

    -- Compile a main program
    r2 <- compileExecutable "main" [ "copilot_test.o" ]
    callProcess "./main" []

    -- Remove file produced by GCC
    removeFile "copilot_test.o"
    removeFile "main"

    -- Remove files produced "by hand"
    removeFile "main.c"

    -- Remove files produced by Copilot
    removeFile "copilot_test.c"
    removeFile "copilot_test.h"
    removeFile "copilot_test_types.h"

    setCurrentDirectory tmpDir
    removeDirectory testDir

    return $ r && r2

  where

    spec = Spec streams observers triggers properties

    streams    = [ Stream 0 [1] (Const Int8 1) Int8]
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "nop"

    guard = Const Bool True

    args = []

-- | Test running compiled spec and comparing the results to the
-- expectation.
testRunCompare :: Property
testRunCompare =
       testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Int8   Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Int16  Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Int32  Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Int64  Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Word8  Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Word16 Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Word32 Bool))
  .&&. testRunCompare1 (arbitraryOpIntegralBool :: Gen (TestCase1 Word64 Bool))
  .&&. testRunCompare1 (arbitraryOpFloatingBool :: Gen (TestCase1 Float  Bool))
  .&&. testRunCompare1 (arbitraryOpFloatingBool :: Gen (TestCase1 Double Bool))
  .&&. testRunCompare2 (arbitraryArrayNum       :: Gen (TestCase2 (Array 2 Int8) Word32 Int8))
  .&&. testRunCompare2 (arbitraryArrayNum       :: Gen (TestCase2 (Array 2 Int16) Word32 Int16))

-- * Random generators

-- ** Random function generators

-- | Generator of functions that produce booleans.
arbitraryOpBool :: Typed a => Gen (Fun a Bool, [a] -> [Bool])
arbitraryOpBool =
  frequency
    [ (5, arbitraryOp1Any)
    , (5, funCompose1 <$> arbitraryOp1Bool <*> arbitraryOpBool)
    , (2, funCompose2 <$> arbitraryOp2Bool <*> arbitraryOpBool <*> arbitraryOpBool)
    , (1, funCompose2 <$> arbitraryOp2Eq   <*> arbitraryOpBool <*> arbitraryOpBool)
    , (1, funCompose2 <$> arbitraryOp2Ord  <*> arbitraryOpBool <*> arbitraryOpBool)
    ]

-- | Generator of functions that take Bits and produce booleans.
arbitraryOpBoolBits :: (Typed a, Bits a) => Gen (Fun a Bool, [a] -> [Bool])
arbitraryOpBoolBits =
  frequency
    [ (1, funCompose2 <$> arbitraryOp2Eq <*> arbitraryOpBits <*> arbitraryOpBits)
    ]

-- | Generator of functions that take Nums and produce booleans.
arbitaryOpBoolOrdEqNum :: (Typed a, Eq a, Ord a, Num a)
                       => Gen (Fun a Bool, [a] -> [Bool])
arbitaryOpBoolOrdEqNum =
  frequency
    [ (1, funCompose2 <$> arbitraryOp2Eq  <*> arbitraryOpNum <*> arbitraryOpNum)
    , (1, funCompose2 <$> arbitraryOp2Ord <*> arbitraryOpNum <*> arbitraryOpNum)
    ]

-- | Generator of functions that take Floating point numbers and produce
-- booleans.
arbitraryOpBoolEqNumFloat :: (Typed t, Eq t, Num t, Floating t)
                          => Gen (Fun t Bool, [t] -> [Bool])
arbitraryOpBoolEqNumFloat =
  frequency
    [ (1, funCompose2 <$> arbitraryOp2Eq <*> arbitraryOpNum   <*> arbitraryOpFloat)
    , (1, funCompose2 <$> arbitraryOp2Eq <*> arbitraryOpFloat <*> arbitraryOpNum)
    ]

-- | Generator of functions that take and produce Bits.
arbitraryOpBits :: (Bits t, Typed t)
                => Gen (Fun t t, [t] -> [t])
arbitraryOpBits = elements
  [ (Op1 (BwNot typeOf), fmap complement)
  ]

-- | Generator of functions that take and produce Nums.
arbitraryOpNum :: (Typed t, Num t) => Gen (Fun t t, [t] -> [t])
arbitraryOpNum = elements
  [ (Op1 (Abs typeOf),  fmap abs)
  , (Op1 (Sign typeOf), fmap signum)
  ]

-- | Generator of functions that take an arrays and indicates and produce
-- elements from the array.
arbitraryArrayIx :: forall t n . (Typed t, KnownNat n, Num t)
                 => Gen ( Fun2 (Array n t) Word32 t
                        , [Array n t] -> [Word32] -> [t]
                        )
arbitraryArrayIx = return
  (Op2 (Index typeOf), zipWith (\x y -> arrayelems x !! fromIntegral y))

-- | Generator of functions on Floating point numbers.
arbitraryOpFloat :: (Floating t, Typed t) => Gen (Fun t t, [t] -> [t])
arbitraryOpFloat = elements
  [ (Op1 (Exp typeOf),   fmap exp)
  , (Op1 (Sqrt typeOf),  fmap sqrt)
  , (Op1 (Log typeOf),   fmap log)
  , (Op1 (Sin typeOf),   fmap sin)
  , (Op1 (Tan typeOf),   fmap tan)
  , (Op1 (Cos typeOf),   fmap cos)
  , (Op1 (Asin typeOf),  fmap asin)
  , (Op1 (Atan typeOf),  fmap atan)
  , (Op1 (Acos typeOf),  fmap acos)
  , (Op1 (Sinh typeOf),  fmap sinh)
  , (Op1 (Tanh typeOf),  fmap tanh)
  , (Op1 (Cosh typeOf),  fmap cosh)
  , (Op1 (Asinh typeOf), fmap asinh)
  , (Op1 (Atanh typeOf), fmap atanh)
  , (Op1 (Acosh typeOf), fmap acosh)
  ]

-- | Generator of functions on that produce elements of any type.
arbitraryOp1Any :: forall a b
                .  (Arbitrary b, Typed a, Typed b)
                => Gen (Fun a b, [a] -> [b])
arbitraryOp1Any = oneof $
    [ (\v -> (\_ -> Const typeOf v, fmap (const v))) <$> arbitrary ]
    ++
    rest
  where
    rest | Just Refl <- testEquality t1 t2
         = [return (id, id)]
         | otherwise
         = []

    t1 :: Type a
    t1 = typeOf

    t2 :: Type b
    t2 = typeOf

-- | Generator of functions on Booleans.
arbitraryOp1Bool :: Gen (Fun Bool Bool, [Bool] -> [Bool])
arbitraryOp1Bool = elements
  [ (Op1 Not, fmap not)
  ]

-- | Generator of binary functions on Booleans.
arbitraryOp2Bool :: Gen (Fun2 Bool Bool Bool, [Bool] -> [Bool] -> [Bool])
arbitraryOp2Bool = elements
  [ (Op2 And, zipWith (&&))
  , (Op2 Or,  zipWith (||))
  ]

-- | Generator of binary functions that take two Eq elements of the same type
-- and return a Bool.
arbitraryOp2Eq :: (Typed t, Eq t)
               => Gen (Fun2 t t Bool, [t] -> [t] -> [Bool])
arbitraryOp2Eq = elements
  [ (Op2 (Eq typeOf), zipWith (==))
  , (Op2 (Ne typeOf), zipWith (/=))
  ]

-- | Generator of binary functions that take two Ord elements of the same type
-- and return a Bool.
arbitraryOp2Ord :: (Typed t, Ord t)
                => Gen (Fun2 t t Bool, [t] -> [t] -> [Bool])
arbitraryOp2Ord = elements
  [ (Op2 (Le typeOf), zipWith (<=))
  , (Op2 (Lt typeOf), zipWith (<))
  , (Op2 (Ge typeOf), zipWith (>=))
  , (Op2 (Gt typeOf), zipWith (>))
  ]

-- ** Random data generators

-- | Random array generator.
arbitraryArray :: forall n t . (KnownNat n, Random t) => Gen (Array n t)
arbitraryArray = array <$> vectorOf len chooseAny
  where
   len :: Int
   len = fromIntegral $ natVal (Proxy :: Proxy n)

-- ** Random test case generators

-- | Generator for test cases on integral numbers that produce booleans.
arbitraryOpIntegralBool :: (Typed t, Bounded t, Integral t, Bits t)
                        => Gen (TestCase1 t Bool)
arbitraryOpIntegralBool = frequency
  [ (5, mkTestCase1
          arbitraryOpBool
          (chooseBoundedIntegral (minBound, maxBound)))

  , (2, mkTestCase1
          arbitraryOpBoolBits
          (chooseBoundedIntegral (minBound, maxBound)))

    -- we need to use +1 because certain operations overflow the number
  , (2, mkTestCase1
          arbitaryOpBoolOrdEqNum
          (chooseBoundedIntegral (minBound + 1, maxBound)))
  ]

-- | Generator for test cases on floating-point numbers that produce booleans.
arbitraryOpFloatingBool :: (Random t, Typed t, Floating t, Eq t)
                        => Gen (TestCase1 t Bool)
arbitraryOpFloatingBool = oneof
  [ mkTestCase1 arbitraryOpBoolEqNumFloat chooseAny
  ]

-- | Generator for test cases on Arrays selection producing values of the
-- array.
arbitraryArrayNum :: forall n a
                  .  (KnownNat n, Num a, Random a, Typed a)
                  => Gen (TestCase2 (Array n a) Word32 a)
arbitraryArrayNum = oneof
    [ mkTestCase2 arbitraryArrayIx arbitraryArray gen
    ]
  where
   gen :: Gen Word32
   gen = choose (0, len - 1)

   len :: Word32
   len = fromIntegral $ natVal (Proxy :: Proxy n)

-- * Semantics

-- ** Functions

-- | Unary Copilot function.
type Fun a b = Expr a -> Expr b

-- | Binary Copilot function.
type Fun2 a b c = Expr a -> Expr b -> Expr c

-- | Compose functions, paired with the Haskell functions that define their
-- idealized meaning.
funCompose1 :: (Fun b c, [b] -> [c])
            -> (Fun a b, [a] -> [b])
            -> (Fun a c, [a] -> [c])
funCompose1 (f1, g1) (f2, g2) = (f1 . f2, g1 . g2)

-- | Compose a binary function, with two functions, one for each argument.
funCompose2 :: (Fun2 b c d, [b] -> [c] -> [d])
            -> (Fun a b, [a] -> [b])
            -> (Fun a c, [a] -> [c])
            -> (Fun a d, [a] -> [d])
funCompose2 (f1, g1) (f2, g2) (f3, g3) =
  (uncurry f1 . (f2 &&& f3), uncurry g1 . (g2 &&& g3))

-- ** Test cases

-- | Test case specification for specs with one input variable and one output.
data TestCase1 a b = TestCase1
  { wrapTC1Expr :: Spec
    -- ^ Specification containing a trigger an extern of type 'a' and a trigger
    -- with an argument of type 'b'.

  , wrapTC1Fun :: [a] -> [b]
    -- ^ Function expected to function in the same way as the Spec being
    -- tested.

  , wrapTC1CopInp :: (String -> String, String, String, Gen a)
    -- ^ Input specification.
    --
    -- - The first element is a function that prints the variable declaration
    -- in C.
    --
    -- - The second element obtains a C expression that calculates the size of
    -- the variable in C.
    --
    -- - The third contains the variable name in C.
    --
    -- - The latter contains a randomized generator for values of the given
    -- type.

  , wrapTC1CopOut :: (String, String)
    -- ^ Output specification.
    --
    -- The first element of the tuple contains the type of the output in C.
    --
    -- The second element of the tuple is the formatting string when printing
    -- values of the given kind.
  }

-- | Test case specification for specs with two input variables and one output.
data TestCase2 a b c = TestCase2
  { wrapTC2Expr :: Spec
    -- ^ Specification containing a trigger an extern of type 'a' and a trigger
    -- with an argument of type 'b'.

  , wrapTC2Fun :: [a] -> [b] -> [c]
    -- ^ Function expected to function in the same way as the Spec being
    -- tested.

  , wrapTC2CopInp1 :: (String -> String, String, String, Gen a)
    -- ^ Input specification for the first input.
    --
    -- - The first element is a function that prints the variable declaration
    -- in C.
    --
    -- - The second element obtains a C expression that calculates the size of
    -- the variable in C.
    --
    -- - The third contains the variable name in C.
    --
    -- - The latter contains a randomized generator for values of the given
    -- type.

  , wrapTC2CopInp2 :: (String -> String, String, String, Gen b)
    -- ^ Input specification for the second input.
    --
    -- - The first element is a function that prints the variable declaration
    -- in C.
    --
    -- - The second element obtains a C expression that calculates the size of
    -- the variable in C.
    --
    -- - The third contains the variable name in C.
    --
    -- - The latter contains a randomized generator for values of the given
    -- type.

  , wrapTC2CopOut :: (String, String)
    -- ^ Output specification.
    --
    -- The first element of the tuple contains the type of the output in C.
    --
    -- The second element of the tuple is the formatting string when printing
    -- values of the given kind.
  }

-- | Generate test cases for expressions that behave like unary functions.
mkTestCase1 :: (Typed a, Typed b)
            => Gen (Fun a b, [a] -> [b])
            -> Gen a
            -> Gen (TestCase1 a b)
mkTestCase1 genO gen = do
    (copilotF, semF) <- genO

    let spec = alwaysTriggerArg1 (UExpr t2 appliedOp)
        appliedOp = copilotF (ExternVar t1 varName Nothing)

    return $
      TestCase1
        spec
        semF
        ( varDeclC t1, sizeC t1, varName, gen )
        ( typeC t2, formatC t2)

  where

    t1 = typeOf
    t2 = typeOf

    varName = "input"

-- | Generate test cases for expressions that behave like binary functions.
mkTestCase2 :: (Typed a, Typed b, Typed c)
            => Gen (Fun2 a b c, [a] -> [b] -> [c])
            -> Gen a
            -> Gen b
            -> Gen (TestCase2 a b c)
mkTestCase2 genO genA genB = do
    (copilotF, semF) <- genO

    let spec = alwaysTriggerArg1 (UExpr t3 appliedOp)
        appliedOp = copilotF (ExternVar t1 varName1 Nothing)
                             (ExternVar t2 varName2 Nothing)

    return $
      TestCase2
        spec
        semF
        ( varDeclC t1, sizeC t1, varName1, genA )
        ( varDeclC t2, sizeC t2, varName2, genB )
        ( typeC t3, formatC t3)

  where

    t1 = typeOf
    t2 = typeOf
    t3 = typeOf

    varName1 = "input1"
    varName2 = "input2"

-- | Test running a compiled C program and comparing the results.
testRunCompare1 :: (Show a, CShow a, ReadableFromC b, Eq b)
                => Gen (TestCase1 a b) -> Property
testRunCompare1 ops =
  forAllBlind ops $ \testCase ->
    let (TestCase1 copilotSpec haskellFun inputVar outputVar) = testCase
        (cTypeInput, size, cInputName, gen) = inputVar

    in forAll (getPositive <$> arbitrary) $ \len ->

         forAll (vectorOf len gen) $ \nums -> do

         let inputs  = [ (cTypeInput, size, fmap cshow nums, cInputName) ]
             outputs = haskellFun nums

         testRunCompareArg inputs len outputs copilotSpec outputVar

-- | Test running a compiled C program and comparing the results.
testRunCompare2 :: (Show a1, CShow a1, Show a2, CShow a2, ReadableFromC b, Eq b)
                => Gen (TestCase2 a1 a2 b) -> Property
testRunCompare2 ops =
  forAllBlind ops $ \testCase ->
    let (TestCase2 copilotSpec haskellFun inputVar1 inputVar2 outputVar) =
          testCase

        (cTypeInput1, size1, cInputName1, gen1) = inputVar1
        (cTypeInput2, size2, cInputName2, gen2) = inputVar2

    in forAll (getPositive <$> arbitrary) $ \len ->
       forAll (vectorOf len gen1) $ \nums1 ->
       forAll (vectorOf len gen2) $ \nums2 -> do
         let inputs = [ (cTypeInput1, size1, fmap cshow nums1, cInputName1)
                      , (cTypeInput2, size2, fmap cshow nums2, cInputName2)
                      ]

             outputs = haskellFun nums1 nums2

         testRunCompareArg inputs len outputs copilotSpec outputVar

-- | Test running a compiled C program and comparing the results, when the
-- program produces one output as an argument to a trigger that always fires.
--
-- PRE: all lists (second argument) of inputs have the length given as second
-- argument.
--
-- PRE: the monitoring code this is linked against uses the function
-- @printBack@ with exactly one argument to pass the results.
testRunCompareArg :: (ReadableFromC b, Eq b)
                  => [(String -> String, String, [String], String)]
                  -> Int
                  -> [b]
                  -> Spec
                  -> (String, String)
                  -> Property
testRunCompareArg inputs numInputs nums spec outputVar =
  ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    -- Operate in temporary directory
    testDir <- mkdtemp "copilot_test_"
    setCurrentDirectory testDir

    -- Produce copilot monitoring code
    compile "copilot_test" spec
    r <- compileC "copilot_test"

    -- Produce wrapper program
    let cProgram = testRunCompareArgCProgram inputs numInputs outputVar
    writeFile "main.c" cProgram

    -- Compile main program
    r2 <- compileExecutable "main" [ "copilot_test.o" ]

    -- Print result so far (for debugging purposes only)
    print r2
    print testDir

    -- Run program and compare result
    out <- readProcess "./main" [] ""
    let outNums = readFromC <$> lines out
        comparison = outNums == nums

    -- Only clean up if the test succeeded; otherwise, we want to inspect it.
    when comparison $ do
      -- Remove file produced by GCC
      removeFile "copilot_test.o"
      removeFile "main"

      -- Remove files produced "by hand"
      removeFile "main.c"

      -- Remove files produced by Copilot
      removeFile "copilot_test.c"
      removeFile "copilot_test.h"
      removeFile "copilot_test_types.h"

      -- Remove temporary directory
      setCurrentDirectory tmpDir
      removeDirectory testDir

    return $ r && r2 && comparison

-- | Return a wrapper C program that runs a loop for a number of iterations,
-- putting values in global variables at every step, running the monitors, and
-- publishing the results of any outputs.
testRunCompareArgCProgram
  :: [(String -> String, String, [String], String)]
  -> Int
  -> (String, String)
  -> String
testRunCompareArgCProgram inputs numSteps outputVar = unlines $
    [ "#include <stdio.h>"
    , "#include <stdint.h>"
    , "#include <stdbool.h>"
    , "#include <string.h>"
    , "#include <inttypes.h>"
    , "#include \"copilot_test.h\""
    , ""
    ]
    ++ varDecls ++
    [ ""
    , "void printBack (" ++ cTypeRes ++ " num) {"
    , "  printf(\"" ++ cStr ++ "\\n\", num);"
    , "}"
    , ""
    , "int main () {"
    , "  int i = 0;"
    , "  for (i = 0; i < " ++ maxInputsName ++ "; i++) {"
    ]
    ++ inputUpdates ++
    [ ""
    , "    step();"
    , "  }"
    , "  return 0;"
    , "}"
    ]

  where

    varDecls :: [String]
    varDecls =
      [ "int " ++ maxInputsName ++ " = " ++ show numSteps ++ ";" ]
      ++ inputVarDecls

    inputVarDecls :: [String]
    inputVarDecls =
      concatMap
        (\(ctypeF, _size, varName, arrVar, arrVals) ->

          let inputsStr = intercalate ", " (arrVals :: [String])

          in [ ctypeF (arrVar ++ "[]") ++ " = {" ++ inputsStr ++ "};"
             , ""
             , ctypeF varName ++ ";"
             ]
        )
        vars

    inputUpdates :: [String]
    inputUpdates = concatMap (\(_ctype, size, varName, arrVar, _arrVals) ->
      [ "    memcpy(&" ++ varName ++ ", &" ++ arrVar ++ "[i], " ++ size ++ ");"
      ])
      vars

    (cTypeRes, cStr) = outputVar

    vars = map oneInput inputs
    oneInput (cTypeInput, size, inputVals, cInputName) =
        (cTypeInput, size, inputVarName, inputArrVarName, inputVals)
      where
        inputVarName    = cInputName
        inputArrVarName = cInputName ++ "_s"

    maxInputsName = "MAX_STEPS"

-- * Auxiliary functions

-- ** Specs handling

-- | Build a 'Spec' that triggers at every step, passing the given expression
-- as argument, and execution a function 'printBack'.
alwaysTriggerArg1 :: UExpr -> Spec
alwaysTriggerArg1 = triggerArg1 (Const Bool True)

  where

    -- | Build a 'Spec' that triggers based on a given boolean stream, passing
    -- the given expression as argument, and execution a function 'printBack'.
    triggerArg1 :: Expr Bool -> UExpr -> Spec
    triggerArg1 guard expr = Spec streams observers triggers properties

      where

        streams    = []
        observers  = []
        properties = []

        triggers = [ Trigger function guard args ]
        function = "printBack"
        args     = [ expr ]

-- ** Compilation of C programs

-- | Compile a C file given its basename.
compileC :: String -> IO Bool
compileC baseName = do
  result <- catch (do callProcess "gcc" [ "-c", baseName ++ ".c" ]
                      return True
                  )
                  (\e -> do
                     hPutStrLn stderr $
                       "copilot-c99: error: compileC: cannot compile "
                         ++ baseName ++ ".c with gcc"
                     hPutStrLn stderr $
                       "copilot-c99: exception: " ++ show (e :: IOException)
                     return False
                  )
  if result
    then doesFileExist $ baseName ++ ".o"
    else return False

-- | Compile a C file into an executable, given its basename and files to link
-- with it.
compileExecutable :: String -> [String] -> IO Bool
compileExecutable baseName linked = do
  result <- catch (do callProcess "gcc" $ [ baseName ++ ".c" ]
                                          ++ linked
                                          ++ [ "-lm" ]
                                          ++ [ "-o", baseName ]
                      return True
                  )
                  (\e -> do
                     hPutStrLn stderr $
                       "copilot-c99: error: compileExecutable: cannot compile "
                         ++ baseName ++ ".c with gcc"
                     hPutStrLn stderr $
                       "copilot-c99: exception: " ++ show (e :: IOException)
                     return False
                  )
  if result
    then doesFileExist baseName
    else return False

-- ** Interfacing between Haskell and C

-- | C formatting string that can be used to print values of a given type.
formatC :: Typed a => Type a -> String
formatC Bool   = "%d"
formatC Int8   = "%d"
formatC Int16  = "%d"
formatC Int32  = "%d"
formatC Int64  = "%ld"
formatC Word8  = "%d"
formatC Word16 = "%d"
formatC Word32 = "%d"
formatC Word64 = "%ld"
formatC Float  = "%f"
formatC Double = "%lf"
formatC _      = error
  "copilot-c99 (test): Printing of arrays and structs is not yet supported."

-- | C type used to store values of a given type.
typeC :: Typed a => Type a -> String
typeC Bool       = "bool"
typeC Int8       = "int8_t"
typeC Int16      = "int16_t"
typeC Int32      = "int32_t"
typeC Int64      = "int64_t"
typeC Word8      = "uint8_t"
typeC Word16     = "uint16_t"
typeC Word32     = "uint32_t"
typeC Word64     = "uint64_t"
typeC Float      = "float"
typeC Double     = "double"
typeC (Array tE) = typeC tE ++ "[]"
typeC _          = error
  "copilot-c99 (test): Input variables of type struct are not yet supported."

-- | C variable declaration for values of a given type.
varDeclC :: Typed a => Type a -> String -> String
varDeclC Bool         v = "bool " ++ v
varDeclC Int8         v = "int8_t " ++ v
varDeclC Int16        v = "int16_t " ++ v
varDeclC Int32        v = "int32_t " ++ v
varDeclC Int64        v = "int64_t " ++ v
varDeclC Word8        v = "uint8_t " ++ v
varDeclC Word16       v = "uint16_t " ++ v
varDeclC Word32       v = "uint32_t " ++ v
varDeclC Word64       v = "uint64_t " ++ v
varDeclC Float        v = "float " ++ v
varDeclC Double       v = "double " ++ v
varDeclC t@(Array tE) v =
  typeC tE ++ " " ++ v ++ "[" ++ show (typeLength t) ++ "]"
varDeclC _            _ = error
  "copilot-c99 (test): Input variables of type struct are not yet supported."

-- | Expression that calculates the size of a variable of a given type.
sizeC :: Typed a => Type a -> String
sizeC Bool         = "sizeof(bool)"
sizeC Int8         = "sizeof(int8_t)"
sizeC Int16        = "sizeof(int16_t)"
sizeC Int32        = "sizeof(int32_t)"
sizeC Int64        = "sizeof(int64_t)"
sizeC Word8        = "sizeof(uint8_t)"
sizeC Word16       = "sizeof(uint16_t)"
sizeC Word32       = "sizeof(uint32_t)"
sizeC Word64       = "sizeof(uint64_t)"
sizeC Float        = "sizeof(float)"
sizeC Double       = "sizeof(double)"
sizeC t@(Array tE) = show (typeLength t) ++ "* sizeof(" ++ typeC tE ++ ")"
sizeC _            = error
  "copilot-c99 (test): Input variables of type struct are not yet supported."

-- | Show a value of a given type in C.
class CShow s where
  cshow :: s -> String

instance CShow Int8 where
  cshow = show

instance CShow Int16 where
  cshow = show

instance CShow Int32 where
  cshow = show

instance CShow Int64 where
  cshow = show

instance CShow Word8 where
  cshow = show

instance CShow Word16 where
  cshow = show

instance CShow Word32 where
  cshow = show

instance CShow Word64 where
  cshow = show

instance CShow Float where
  cshow = show

instance CShow Double where
  cshow = show

instance CShow Bool where
  cshow True  = "true"
  cshow False = "false"

instance CShow t => CShow (Array n t) where
  cshow a = intercalate "," $ map cshow $ arrayelems a

-- | Read a value of a given type in C.
class ReadableFromC a where
  readFromC :: String -> a

instance ReadableFromC Bool where
  readFromC "0" = False
  readFromC _   = True

instance ReadableFromC Int8 where
  readFromC = read

instance ReadableFromC Int16 where
  readFromC = read

instance ReadableFromC Int32 where
  readFromC = read

instance ReadableFromC Int64 where
  readFromC = read

instance ReadableFromC Word8 where
  readFromC = read

instance ReadableFromC Word16 where
  readFromC = read

instance ReadableFromC Word32 where
  readFromC = read

instance ReadableFromC Word64 where
  readFromC = read
