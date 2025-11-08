{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Test copilot-bluespec:Copilot.Compile.Bluespec.
module Test.Copilot.Compile.Bluespec
    ( tests )
  where

-- External imports
import Control.Arrow                        ((&&&))
import Control.Exception                    (IOException, catch)
import Control.Monad                        (when)
import Data.AEq                             (AEq (..))
import Data.Bits                            (Bits, complement)
import Data.Foldable                        (foldl')
import Data.List                            (intercalate, stripPrefix)
import Data.List.Extra                      (stripSuffix)
import Data.Type.Equality                   (testEquality)
import Data.Typeable                        (Proxy (..), (:~:) (Refl))
import GHC.Float                            (castDoubleToWord64,
                                             castFloatToWord32,
                                             castWord32ToFloat,
                                             castWord64ToDouble)
import GHC.TypeLits                         (KnownNat, natVal)
import Numeric.IEEE                         (infinity, nan)
import System.Directory                     (doesFileExist,
                                             getTemporaryDirectory,
                                             removeDirectoryRecursive,
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
                                             getPositive, ioProperty, once,
                                             oneof, vectorOf, withMaxSuccess,
                                             (.&&.))
import Test.QuickCheck.Gen                  (chooseAny, chooseBoundedIntegral)
import Text.ParserCombinators.ReadPrec      (minPrec)

-- External imports: Copilot
import Copilot.Core hiding (Property)

-- External imports
import Copilot.Compile.Bluespec (bluespecSettingsOutputDirectory, compile,
                                 compileWith, mkDefaultBluespecSettings)

-- Internal imports
import Test.Copilot.Compile.Bluespec.External (External (extName), gatherExts)

-- * Constants

-- | All tests for copilot-bluespec:Copilot.Compile.Bluespec.
tests :: Test.Framework.Test
tests =
  testGroup "Copilot.Compile.Bluespec"
    [ testGroup "Unit tests"
      [ testProperty "Compile specification"               testCompile
      , testProperty "Compile specification in custom dir" testCompileCustomDir
      , testProperty "Run specification"                   testRun
      , testProperty "Run and compare results"             testRunCompare
      ]
    , testGroup "Regression tests"
      [ test14
      , test15
      ]
    ]

-- * Individual tests

-- | Test compiling a spec.
testCompile :: Property
testCompile = once $ ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "CopilotTest"
    setCurrentDirectory testDir

    compile "CopilotTest" spec
    r <- compileBluespec "CopilotTest" []

    setCurrentDirectory tmpDir
    removeDirectoryRecursive testDir

    return r

  where

    spec = Spec streams observers triggers properties

    streams    = []
    observers  = []
    triggers   = [ Trigger function guard args ]
    properties = []

    function = "func"

    guard = Const Bool True

    args = []

-- | Test compiling a spec in a custom directory.
testCompileCustomDir :: Property
testCompileCustomDir = once $ ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "CopilotTest"

    compileWith (mkDefaultBluespecSettings
                   { bluespecSettingsOutputDirectory = testDir })
                "CopilotTest"
                spec

    setCurrentDirectory testDir
    r <- compileBluespec "CopilotTest" []

    setCurrentDirectory tmpDir
    removeDirectoryRecursive testDir

    return r

  where

    spec = Spec streams observers triggers properties

    streams    = []
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
testRun = once $ ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    testDir <- mkdtemp "CopilotTest"
    setCurrentDirectory testDir

    let bluespecProgram = unlines
          [ "package Top where"
          , ""
          , "import CopilotTest"
          , "import CopilotTestTypes"
          , ""
          , "mkTop :: Module Empty"
          , "mkTop ="
          , "  module"
          , "    copilotTestMod <- mkCopilotTest"
          , "    addCopilotTestRules copilotTestMod $"
          , "      interface CopilotTestRulesIfc"
          , "        nop_action = return ()"
          ]

    writeFile "Top.bs" bluespecProgram

    compile "CopilotTest" spec
    r <- compileBluespec "Top" ["-g", "mkTop"]

    -- Compile a main program
    r2 <- compileExecutable "mkTop"
    callProcess "./mkTop" ["-m", "2"]

    setCurrentDirectory tmpDir
    removeDirectoryRecursive testDir

    return $ r && r2

  where

    spec = Spec streams observers triggers properties

    streams    = []
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
  -- It takes a pretty long time to run these tests, so we set the maximum
  -- number of successful tests to 5 instead of the default (100) for the sake
  -- of making the test suite complete in a more reasonable amount of time.
  withMaxSuccess 5 $
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
  .&&. testRunCompare1 (arbitraryOpStruct       :: Gen (TestCase1 MyStruct Int8))
  .&&. testRunCompare2 (arbitraryOp2Struct      :: Gen (TestCase2 MyStruct Int8 MyStruct))
  .&&. testRunCompare2 (arbitraryArray2Num      :: Gen (TestCase2 (Array 2 Int8) Word32 Int8))
  .&&. testRunCompare2 (arbitraryArray2Num      :: Gen (TestCase2 (Array 2 Int16) Word32 Int16))
  .&&. testRunCompare3 (arbitraryArray3Num      :: Gen (TestCase3 (Array 2 Int16) Word32 Int16 (Array 2 Int16)))

-- * Regression tests

-- | Regression tests for
-- https://github.com/Copilot-Language/copilot-bluespec/issues/14 which ensure
-- that @copilot-bluespec@ generates code for the @signum@ function that adheres
-- to Copilot's @signum@ semantics.
test14 :: Test.Framework.Test
test14 =
  testGroup "#14"
    [ testProperty "`signum @Int8` generates correct Bluespec code" $
      mkRegressionTest1 (Sign Int8) (fmap signum)
        [-2, -1, 0, 1, 2]
    , testProperty "`signum @Double` generates correct Bluespec code" $
      mkRegressionTest1 (Sign Double) (fmap signum)
        [-nan, -infinity, -2, -1, -0.0, 0, 1, 2, infinity, nan]
    ]

-- | Regression tests for
-- https://github.com/Copilot-Language/copilot-bluespec/issues/15 which ensure
-- that @copilot-bluespec@ generates valid code for comparison operators (('<'),
-- ('<='), ('>'), and ('>=')) that are capable of handling NaN values.
test15 :: Test.Framework.Test
test15 =
  testGroup "#15"
    [ testProperty "Generates valid (<) code for NaNs" $
      mkRegressionTest2 (Lt Double) (zipWith (<)) vals
    , testProperty "Generates valid (<=) code for NaNs" $
      mkRegressionTest2 (Le Double) (zipWith (<=)) vals
    , testProperty "Generates valid (>) code for NaNs" $
      mkRegressionTest2 (Gt Double) (zipWith (>)) vals
    , testProperty "Generates valid (>=) code for NaNs" $
      mkRegressionTest2 (Ge Double) (zipWith (>=)) vals
    ]
  where
    vals :: [(Double, Double)]
    vals = [(0, nan), (nan, 0)]

-- | Test the behavior of a unary operation (an @'Op1' a b@ value) against its
-- expected behavior (as a Haskell function of type @[a] -> [b]@) using the
-- supplied inputs (of type @[a]@). This function is intended to be used to
-- construct regression tests.
mkRegressionTest1 :: (Typed a, Typed b,
                      DisplayableInBluespec b, ReadableFromBluespec b, AEq b)
                  => Op1 a b
                  -> ([a] -> [b])
                  -> [a]
                  -> Property
mkRegressionTest1 op haskellFun vals =
    let spec = alwaysTriggerArg1 (UExpr t2 appliedOp)
        appliedOp = Op1 op (ExternVar t1 varName Nothing)

        len = length vals
        inputs  = filterOutUnusedExts
                    spec
                    [ (typeBluespec t1,
                       fmap (bluespecShow t1) vals,
                       varName)
                    ]
        outputs = haskellFun vals in

    once $
    testRunCompareArg
      inputs len outputs spec (typeBluespec t2)

  where

    t1 = typeOf
    t2 = typeOf

    varName = "input1"

-- | Test the behavior of a binary operation (an @'Op2' a b c@ value) against
-- its expected behavior (as a Haskell function of type @[a] -> [b] -> [c]@)
-- using the supplied inputs (of type @[(a, b)]@). This function is intended to
-- be used to construct regression tests.
mkRegressionTest2 :: (Typed a, Typed b, Typed c,
                      DisplayableInBluespec c, ReadableFromBluespec c, AEq c)
                  => Op2 a b c
                  -> ([a] -> [b] -> [c])
                  -> [(a, b)]
                  -> Property
mkRegressionTest2 op haskellFun vals =
    let spec = alwaysTriggerArg1 (UExpr t3 appliedOp)
        appliedOp = Op2 op (ExternVar t1 varName1 Nothing)
                           (ExternVar t2 varName2 Nothing)

        len = length vals
        (vals1, vals2) = unzip vals
        inputs  = filterOutUnusedExts
                    spec
                    [ (typeBluespec t1,
                       fmap (bluespecShow t1) vals1,
                       varName1)
                    , (typeBluespec t2,
                       fmap (bluespecShow t2) vals2,
                       varName2)
                    ]
        outputs = haskellFun vals1 vals2 in

    once $
    testRunCompareArg
      inputs len outputs spec (typeBluespec t3)

  where

    t1 = typeOf
    t2 = typeOf
    t3 = typeOf

    varName1 = "input1"
    varName2 = "input2"

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
  (Op2 (Index typeOf), zipWith (\x y -> arrayElems x !! fromIntegral y))

-- | Generator of functions that take arrays, indices, and values, and then
-- produce new arrays by updating the elements at the indices with the values.
arbitraryArrayUpdate :: forall t n . (Typed t, KnownNat n, Num t)
                     => Gen ( Fun3 (Array n t) Word32 t (Array n t)
                            , [Array n t] -> [Word32] -> [t] -> [Array n t]
                            )
arbitraryArrayUpdate = return
  (Op3 (UpdateArray typeOf), zipWith3 (\x y z -> array (updateAt y z (arrayElems x))))
  where
    updateAt :: forall a. Word32 -> a -> [a] -> [a]
    updateAt _ _ [] = []
    updateAt 0 x (_ : as) = x : as
    updateAt n x (a : as) = a : updateAt (n-1) x as

-- | Generator of functions that take structs produce fields of the struct.
arbitraryStructField :: Gen ( Fun MyStruct Int8
                            , [MyStruct] -> [Int8]
                            )
arbitraryStructField = elements
  [ (Op1 (GetField typeOf typeOf myStruct1), fmap (unField . myStruct1))
  , (Op1 (GetField typeOf typeOf myStruct2), fmap (unField . myStruct2))
  ]

-- | Generator of functions that take and produce structs, where the returned
-- structs have one field value updated.
arbitraryStructUpdate :: Gen ( Fun2 MyStruct Int8 MyStruct
                             , [MyStruct] -> [Int8] -> [MyStruct]
                             )
arbitraryStructUpdate = elements
  [ ( Op2 (UpdateField typeOf typeOf myStruct1)
    , zipWith (\s i -> s { myStruct1 = Field i })
    )
  , ( Op2 (UpdateField typeOf typeOf myStruct2)
    , zipWith (\s i -> s { myStruct2 = Field i })
    )
  ]

-- | Generator of functions on Floating point numbers.
arbitraryOpFloat :: (Floating t, Typed t) => Gen (Fun t t, [t] -> [t])
arbitraryOpFloat = elements
  [ (Op1 (Recip typeOf), fmap recip)
  , (Op1 (Exp typeOf),   fmap exp)
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

-- | Random struct generator.
arbitraryStruct :: Gen MyStruct
arbitraryStruct = do
   fld1 <- Field <$> gen
   fld2 <- Field <$> gen
   return $ MyStruct fld1 fld2
  where
   gen :: Gen Int8
   gen = chooseBoundedIntegral (minBound, maxBound)

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
arbitraryArray2Num :: forall n a
                  .  (KnownNat n, Num a, Random a, Typed a)
                  => Gen (TestCase2 (Array n a) Word32 a)
arbitraryArray2Num = oneof
    [ mkTestCase2 arbitraryArrayIx arbitraryArray gen
    ]
  where
   gen :: Gen Word32
   gen = choose (0, len - 1)

   len :: Word32
   len = fromIntegral $ natVal (Proxy :: Proxy n)

-- | Generator for test cases on Arrays which update values of the array.
arbitraryArray3Num :: forall n a
                   .  (KnownNat n, Num a, Random a, Typed a)
                   => Gen (TestCase3 (Array n a) Word32 a (Array n a))
arbitraryArray3Num = oneof
    [ mkTestCase3 arbitraryArrayUpdate arbitraryArray gen chooseAny
    ]
  where
   gen :: Gen Word32
   gen = choose (0, len - 1)

   len :: Word32
   len = fromIntegral $ natVal (Proxy :: Proxy n)

-- | Generator for test cases on structs that produce fields of the struct.
arbitraryOpStruct :: Gen (TestCase1 MyStruct Int8)
arbitraryOpStruct = oneof
    [ mkTestCase1
        arbitraryStructField
        arbitraryStruct
    ]

-- | Generator for test cases that take and produce structs, where the returned
-- structs have one field value updated.
arbitraryOp2Struct :: Gen (TestCase2 MyStruct Int8 MyStruct)
arbitraryOp2Struct = oneof
    [ mkTestCase2
        arbitraryStructUpdate
        arbitraryStruct
        gen
    ]
  where
   gen :: Gen Int8
   gen = chooseBoundedIntegral (minBound, maxBound)

-- * Semantics

-- ** Functions

-- | Unary Copilot function.
type Fun a b = Expr a -> Expr b

-- | Binary Copilot function.
type Fun2 a b c = Expr a -> Expr b -> Expr c

-- | Ternary Copilot function.
type Fun3 a b c d = Expr a -> Expr b -> Expr c -> Expr d

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
    -- ^ Specification containing a trigger with an extern of type 'a'.

  , wrapTC1Fun :: [a] -> [b]
    -- ^ Function expected to function in the same way as the Spec being
    -- tested.

  , wrapTC1CopInp :: (Type a, String, Gen a)
    -- ^ Input specification.
    --
    -- - The first element contains the type of the input in Bluespec.
    --
    -- - The second contains the variable name in Bluespec.
    --
    -- - The latter contains a randomized generator for values of the given
    -- type.

  , wrapTC1CopOut :: Type b
    -- ^ The type of the output in Bluespec.
  }

-- | Test case specification for specs with two input variables and one output.
data TestCase2 a b c = TestCase2
  { wrapTC2Expr :: Spec
    -- ^ Specification containing a trigger with an extern of type 'a' and a
    -- trigger with an argument of type 'b'.

  , wrapTC2Fun :: [a] -> [b] -> [c]
    -- ^ Function expected to function in the same way as the Spec being
    -- tested.

  , wrapTC2CopInp1 :: (Type a, String, Gen a)
    -- ^ Input specification for the first input.
    --
    -- - The first element contains the type of the input in Bluespec.
    --
    -- - The second contains the variable name in Bluespec.
    --
    -- - The latter contains a randomized generator for values of the given
    -- type.

  , wrapTC2CopInp2 :: (Type b, String, Gen b)
    -- ^ Input specification for the second input.
    --
    -- - The first element contains the type of the input in Bluespec.
    --
    -- - The second contains the variable name in Bluespec.
    --
    -- - The latter contains a randomized generator for values of the given
    -- type.

  , wrapTC2CopOut :: Type c
    -- ^ The type of the output in Bluespec.
  }

-- | Test case specification for specs with three input variables and one output.
data TestCase3 a b c d = TestCase3
  { wrapTC3Expr :: Spec
    -- ^ Specification containing a trigger with an extern of type 'a', a
    -- trigger with an argument of type 'b', and a trigger with an argument of
    -- type 'c'.

  , wrapTC3Fun :: [a] -> [b] -> [c] -> [d]
    -- ^ Function expected to function in the same way as the Spec being
    -- tested.

  , wrapTC3CopInp1 :: (Type a, String, Gen a)
    -- ^ Input specification for the first input.
    --
    -- - The first element contains the type of the input in Bluespec.
    --
    -- - The second contains the variable name in Bluespec.
    --
    -- - The latter contains a randomized generator for values of the given
    -- type.

  , wrapTC3CopInp2 :: (Type b, String, Gen b)
    -- ^ Input specification for the second input.
    --
    -- - The first element contains the type of the input in Bluespec.
    --
    -- - The second contains the variable name in Bluespec.
    --
    -- - The latter contains a randomized generator for values of the given
    -- type.

  , wrapTC3CopInp3 :: (Type c, String, Gen c)
    -- ^ Input specification for the second input.
    --
    -- - The first element contains the type of the input in Bluespec.
    --
    -- - The second contains the variable name in Bluespec.
    --
    -- - The latter contains a randomized generator for values of the given
    -- type.

  , wrapTC3CopOut :: Type d
    -- ^ The type of the output in Bluespec.
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
        { wrapTC1Expr = spec
        , wrapTC1Fun = semF
        , wrapTC1CopInp = ( t1, varName, gen )
        , wrapTC1CopOut = t2
        }

  where

    t1 = typeOf
    t2 = typeOf

    varName = "input1"

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
        { wrapTC2Expr = spec
        , wrapTC2Fun = semF
        , wrapTC2CopInp1 = ( t1, varName1, genA )
        , wrapTC2CopInp2 = ( t2, varName2, genB )
        , wrapTC2CopOut = t3
        }

  where

    t1 = typeOf
    t2 = typeOf
    t3 = typeOf

    varName1 = "input1"
    varName2 = "input2"

-- | Generate test cases for expressions that behave like ternary functions.
mkTestCase3 :: (Typed a, Typed b, Typed c, Typed d)
            => Gen (Fun3 a b c d, [a] -> [b] -> [c] -> [d])
            -> Gen a
            -> Gen b
            -> Gen c
            -> Gen (TestCase3 a b c d)
mkTestCase3 genO genA genB genC = do
    (copilotF, semF) <- genO

    let spec = alwaysTriggerArg1 (UExpr t4 appliedOp)
        appliedOp = copilotF (ExternVar t1 varName1 Nothing)
                             (ExternVar t2 varName2 Nothing)
                             (ExternVar t3 varName3 Nothing)

    return $
      TestCase3
        { wrapTC3Expr = spec
        , wrapTC3Fun = semF
        , wrapTC3CopInp1 = ( t1, varName1, genA )
        , wrapTC3CopInp2 = ( t2, varName2, genB )
        , wrapTC3CopInp3 = ( t3, varName3, genC )
        , wrapTC3CopOut = t4
        }

  where

    t1 = typeOf
    t2 = typeOf
    t3 = typeOf
    t4 = typeOf

    varName1 = "input1"
    varName2 = "input2"
    varName3 = "input3"

-- | Test running a compiled Bluespec program and comparing the results.
testRunCompare1 :: (Show a, Typed a,
                    DisplayableInBluespec b, ReadableFromBluespec b,
                    AEq b, Typed b)
                => Gen (TestCase1 a b) -> Property
testRunCompare1 ops =
  forAllBlind ops $ \testCase ->
    let (TestCase1
           { wrapTC1Expr = copilotSpec
           , wrapTC1Fun = haskellFun
           , wrapTC1CopInp = inputVar
           , wrapTC1CopOut = outputType
           }) = testCase
        (bluespecTypeInput, bluespecInputName, gen) = inputVar

    in forAll (getPositive <$> arbitrary) $ \len ->

         forAll (vectorOf len gen) $ \vals -> do

         let inputs  = filterOutUnusedExts
                         copilotSpec
                         [ (typeBluespec bluespecTypeInput,
                            fmap (bluespecShow bluespecTypeInput) vals,
                            bluespecInputName)
                         ]
             outputs = haskellFun vals

         testRunCompareArg
           inputs len outputs copilotSpec (typeBluespec outputType)

-- | Test running a compiled Bluespec program and comparing the results.
testRunCompare2 :: (Show a1, Typed a1, Show a2, Typed a2,
                    DisplayableInBluespec b, ReadableFromBluespec b,
                    AEq b, Typed b)
                => Gen (TestCase2 a1 a2 b) -> Property
testRunCompare2 ops =
  forAllBlind ops $ \testCase ->
    let (TestCase2
           { wrapTC2Expr = copilotSpec
           , wrapTC2Fun = haskellFun
           , wrapTC2CopInp1 = inputVar1
           , wrapTC2CopInp2 = inputVar2
           , wrapTC2CopOut = outputType
           }) =
          testCase

        (bluespecTypeInput1, bluespecInputName1, gen1) = inputVar1
        (bluespecTypeInput2, bluespecInputName2, gen2) = inputVar2

    in forAll (getPositive <$> arbitrary) $ \len ->
       forAll (vectorOf len gen1) $ \vals1 ->
       forAll (vectorOf len gen2) $ \vals2 -> do
         let inputs  = filterOutUnusedExts
                         copilotSpec
                         [ (typeBluespec bluespecTypeInput1,
                            fmap (bluespecShow bluespecTypeInput1) vals1,
                            bluespecInputName1)
                         , (typeBluespec bluespecTypeInput2,
                            fmap (bluespecShow bluespecTypeInput2) vals2,
                            bluespecInputName2)
                         ]
             outputs = haskellFun vals1 vals2

         testRunCompareArg
           inputs len outputs copilotSpec (typeBluespec outputType)

-- | Test running a compiled Bluespec program and comparing the results.
testRunCompare3 :: (Show a1, Typed a1, Show a2, Typed a2, Show a3, Typed a3,
                    DisplayableInBluespec b, ReadableFromBluespec b,
                    AEq b, Typed b)
                => Gen (TestCase3 a1 a2 a3 b) -> Property
testRunCompare3 ops =
  forAllBlind ops $ \testCase ->
    let (TestCase3
           { wrapTC3Expr = copilotSpec
           , wrapTC3Fun = haskellFun
           , wrapTC3CopInp1 = inputVar1
           , wrapTC3CopInp2 = inputVar2
           , wrapTC3CopInp3 = inputVar3
           , wrapTC3CopOut = outputType
           }) =
          testCase

        (bluespecTypeInput1, bluespecInputName1, gen1) = inputVar1
        (bluespecTypeInput2, bluespecInputName2, gen2) = inputVar2
        (bluespecTypeInput3, bluespecInputName3, gen3) = inputVar3

    in forAll (getPositive <$> arbitrary) $ \len ->
       forAll (vectorOf len gen1) $ \vals1 ->
       forAll (vectorOf len gen2) $ \vals2 ->
       forAll (vectorOf len gen3) $ \vals3 -> do
         let inputs  = filterOutUnusedExts
                         copilotSpec
                         [ (typeBluespec bluespecTypeInput1,
                            fmap (bluespecShow bluespecTypeInput1) vals1,
                            bluespecInputName1)
                         , (typeBluespec bluespecTypeInput2,
                            fmap (bluespecShow bluespecTypeInput2) vals2,
                            bluespecInputName2)
                         , (typeBluespec bluespecTypeInput3,
                            fmap (bluespecShow bluespecTypeInput3) vals3,
                            bluespecInputName3)
                         ]
             outputs = haskellFun vals1 vals2 vals3

         testRunCompareArg
           inputs len outputs copilotSpec (typeBluespec outputType)

-- | Test running a compiled Bluespec program and comparing the results, when
-- the program produces one output as an argument to a trigger that always
-- fires.
--
-- PRE: all lists (second argument) of inputs have the length given as second
-- argument.
--
-- PRE: the monitoring code this is linked against uses the function
-- @printBack@ with exactly one argument to pass the results.
testRunCompareArg :: forall b
                   . (DisplayableInBluespec b, ReadableFromBluespec b, AEq b)
                  => [(String, [String], String)]
                  -> Int
                  -> [b]
                  -> Spec
                  -> String
                  -> Property
testRunCompareArg inputs numInputs vals spec outputType =
  ioProperty $ do
    tmpDir <- getTemporaryDirectory
    setCurrentDirectory tmpDir

    -- Operate in temporary directory
    testDir <- mkdtemp "CopilotTest"
    setCurrentDirectory testDir

    -- Produce wrapper program
    let bluespecProgram =
          testRunCompareArgBluespecProgram (Proxy :: Proxy b) inputs outputType
    writeFile "Top.bs" bluespecProgram

    -- Produce copilot monitoring code
    compile "CopilotTest" spec
    r <- compileBluespec "Top" ["-g", "mkTop"]

    -- Compile main program
    r2 <- compileExecutable "mkTop"

    -- Print result so far (for debugging purposes only)
    {-
    print r2
    print testDir
    -}

    -- Run the program and compare the results. Note that we use (===) (using
    -- the `AEq` class from the `ieee754` package) rather than (==) (using the
    -- `Eq` class), as the former allows us to use exact equality comparisons
    -- for floating-point types. This lets us ensure that we are handling NaN
    -- and -0.0 values correctly.
    out <- readProcess "./mkTop" ["-m", show (numInputs + 2)] ""
    let outNums = readFromBluespec <$> lines out
        comparison = outNums === vals

    -- Only clean up if the test succeeded; otherwise, we want to inspect it.
    when comparison $ do
      -- Remove temporary directory
      setCurrentDirectory tmpDir
      removeDirectoryRecursive testDir

    return $ r && r2 && comparison

-- | Return a wrapper Bluespec program that runs for a number of clock cycles,
-- updating external stream registers on every cycle, running the monitors, and
-- publishing the results of any outputs.
testRunCompareArgBluespecProgram
  :: DisplayableInBluespec b
  => Proxy b
  -> [(String, [String], String)]
  -> String
  -> String
testRunCompareArgBluespecProgram proxy inputs outputType = unlines $
    [ "package Top where"
    , ""
    , "import FloatingPoint"
    , "import Vector"
    , ""
    , "import CopilotTest"
    , "import CopilotTestTypes"
    , ""
    ]
    ++ inputVecDecls ++
    [ ""
    , "mkTop :: Module Empty"
    , "mkTop ="
    , "  module"
    , "    copilotTestMod <- mkCopilotTest"
    ]
    ++ inputRegs ++
    [ "    i :: Reg (Bit 64) <- mkReg 0"
    , "    ready :: Reg Bool <- mkReg False"
    , ""
    , "    addCopilotTestRules copilotTestMod $"
    , "      interface CopilotTestRulesIfc"
    , "        printBack_action :: " ++ outputType ++ " -> Action"
    , "        printBack_action output = $display " ++ printBackDisplayArgs
    , "                                  when ready"
    ]
    ++ inputMethods ++
    [ ""
    , "    rules"
    , "      \"inputs\": when True ==> do"
    ]
    ++ inputUpdates ++
    [ "        i := i + 1"
    , "        ready := True"
    ]
  where
    printBackDisplayArgs :: String
    printBackDisplayArgs = unwords (displayInBluespec proxy "output")

    inputVecDecls :: [String]
    inputVecDecls =
      concatMap
        (\(bluespecType, _varName, _regName, inputVecName, inputVals) ->
          [ inputVecName ++ " :: Vector " ++ show (length inputVals) ++
            " (" ++ bluespecType ++ ")"
          , inputVecName ++ " = " ++ genVector inputVals
          ])
        vars

    inputRegs :: [String]
    inputRegs =
      map
        (\(bluespecType, _varName, regName, _inputVecName, _inputVals) ->
          "    " ++ regName ++ " :: Reg (" ++ bluespecType ++ ") <- mkRegU")
        vars

    inputMethods :: [String]
    inputMethods =
      map
        (\(_bluespecType, varName, regName, _inputVecName, _inputVals) ->
          "        " ++ varName ++ "_action = return " ++ regName)
        vars

    inputUpdates :: [String]
    inputUpdates =
      map
        (\(_bluespecType, _varName, regName, inputVecName, _inputVals) ->
          "        " ++ regName ++ " := select " ++ inputVecName ++ " i")
        vars

    vars = map oneInput inputs
    oneInput (bluespecTypeInput, inputVals, bluespecInputName) =
        (bluespecTypeInput, inputVarName, inputRegVarName, inputVecVarName,
         inputVals)
      where
        inputVarName    = bluespecInputName
        inputRegVarName = bluespecInputName ++ "Impl"
        inputVecVarName = bluespecInputName ++ "Inputs"

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

-- | Filter out any elements in the input list (of type @[(a, b, String)]@)
-- whose first element (the name of an external stream) does not correspond to
-- the name of an external stream in the supplied 'Spec'. For example, a Copilot
-- source program may declare external streams, but if none of them are used in
-- the 'Spec', then the 'Spec' value itself will not contain any external stream
-- definitions. As a result, we want to ensure that the input list also does not
-- contain any external streams.
filterOutUnusedExts :: Spec -> [(a, b, String)] -> [(a, b, String)]
filterOutUnusedExts spec = filter (\(_, _, name) -> name `elem` extNames)
  where
    extNames = map extName $ gatherExts (specStreams spec) (specTriggers spec)

-- ** Compilation of Bluespec programs

-- | Compile a Bluespec file given its basename.
compileBluespec :: String -> [String] -> IO Bool
compileBluespec baseName extraArgs = do
  result <- catch (do callProcess "bsc" $ extraArgs ++
                                          [ "-sim", "-quiet", "-u",
                                            -- We suppress the G0023 warning,
                                            -- which arises due to the `nop`
                                            -- triggers defined above. See the
                                            -- DESIGN.md document for more
                                            -- details on what these warning
                                            -- codes mean.
                                            "-suppress-warnings", "G0023:S0080",
                                            baseName ++ ".bs" ]
                      return True
                  )
                  (\e -> do
                     hPutStrLn stderr $
                       "copilot-bluespec: error: compileBluespec: "
                         ++ "cannot compile " ++ baseName ++ ".bs with bsc"
                     hPutStrLn stderr $
                       "copilot-bluespec: exception: " ++ show (e :: IOException)
                     return False
                  )
  if result
    then doesFileExist $ baseName ++ ".bo"
    else return False

-- | Compile a Bluespec file into an executable given its basename.
compileExecutable :: String -> IO Bool
compileExecutable topExe = do
  result <- catch (do callProcess "bsc" [ "-sim", "-quiet"
                                        , "-e", topExe
                                        , "-o", topExe
                                        , "bs_fp.c"
                                        ]
                      return True
                  )
                  (\e -> do
                     hPutStrLn stderr $
                       "copilot-bluespec: error: compileExecutable: "
                         ++ "cannot compile " ++ topExe ++ " with bsc"
                     hPutStrLn stderr $
                       "copilot-bluespec: exception: "
                         ++ show (e :: IOException)
                     return False
                  )
  if result
    then doesFileExist topExe
    else return False

-- ** Interfacing between Haskell and Bluespec

-- | Bluespec type used to store values of a given type.
typeBluespec :: Typed a => Type a -> String
typeBluespec Bool         = "Bool"
typeBluespec Int8         = "Int 8"
typeBluespec Int16        = "Int 16"
typeBluespec Int32        = "Int 32"
typeBluespec Int64        = "Int 64"
typeBluespec Word8        = "UInt 8"
typeBluespec Word16       = "UInt 16"
typeBluespec Word32       = "UInt 32"
typeBluespec Word64       = "UInt 64"
typeBluespec Float        = "Float"
typeBluespec Double       = "Double"
typeBluespec t@(Array tE) =
  "Vector " ++ show (typeLength t)  ++ "(" ++ typeBluespec tE ++ ")"
typeBluespec (Struct s)   = typeName s

-- | Show a value of a given type in Bluespec.
bluespecShow :: Type a -> a -> String
bluespecShow Bool       x = show x
bluespecShow Int8       x = bluespecShowIntegral x
bluespecShow Int16      x = bluespecShowIntegral x
bluespecShow Int32      x = bluespecShowIntegral x
bluespecShow Int64      x = bluespecShowIntegral x
bluespecShow Word8      x = bluespecShowIntegral x
bluespecShow Word16     x = bluespecShowIntegral x
bluespecShow Word32     x = bluespecShowIntegral x
bluespecShow Word64     x = bluespecShowIntegral x
bluespecShow Float      x = bluespecShowRealFloat 32 castFloatToWord32 x
bluespecShow Double     x = bluespecShowRealFloat 64 castDoubleToWord64 x
bluespecShow (Array tE) x = genVector $ map (bluespecShow tE) $ arrayElems x
bluespecShow (Struct s) x =
  typeName s
    ++ "{ "
    ++ intercalate ";"
         (map
           (\(Value fldTy fld@(Field val)) ->
             fieldName fld ++ " = " ++ bluespecShow fldTy val)
           (toValues x))
    ++ "}"

-- | Show a value of a integral type (e.g., 'Int8' or 'Word8').
bluespecShowIntegral :: (Integral a, Num a, Ord a, Show a) => a -> String
bluespecShowIntegral x
  | x >= 0    = show x
  -- Bluespec Haskell doesn't have negative integer literals, so something like
  -- "-42" won't parse. Instead, we must rely on Bluespec's `negate` function.
  --
  -- We must be careful to negate an Integer literal rather than than a
  -- fixed-precision literal. For instance, suppose we wanted to display
  -- (-128 :: Int8). We wouldn't want to do this by displaying `negate 128`,
  -- since 128 isn't a valid Int8 value—the maximum Int8 value is 127!
  -- Instead, we want to display `fromInteger (negate 128)`, where 128 is an
  -- Integer. This way, `negate` can turn `128` to `-128` without issues.
  | otherwise = "fromInteger (negate " ++ show (abs (toInteger x)) ++ ")"

-- | Show a value of a floating-point type (e.g., 'Float' or 'Double'). We make
-- sure to convert NaN and infinity values to the corresponding Bluespec
-- @FloatingPoint@ functions that construct these values.
bluespecShowRealFloat ::
     (Num float, Ord float, RealFloat float, Show float, Show word)
  => Int
  -> (float -> word)
  -> float
  -> String
bluespecShowRealFloat floatSizeInBits castFloatToWord float
    -- We want to ensure that NaN values are correctly translated to Bluespec,
    -- bit by bit. We have two mechanisms to do so. On the Haskell side, we have
    -- `ieee754`'s `nanWithPayload` function, and on the Bluespec side, we have
    -- `FloatingPoint`'s `nanQuiet` function. Unfortunately, their arguments are
    -- of different types, and it isn't quite clear how to express one function
    -- in terms of the other.
    --
    -- To avoid this problem, we take a more indirect approach: we first cast
    -- the Haskell floating-point value to a word and then `unpack` the word to
    -- a floating-point value on the Bluespec side. It's somewhat verbose, but
    -- it gets the job done reliably.
  | isNaN float
  = "unpack (" ++ show (castFloatToWord float) ++
    " :: Bit " ++ show floatSizeInBits ++ ")"

  | isInfinite float
  = "infinity " ++ show floatIsNeg

  | floatIsNeg
  = "negate " ++ show (abs float)

  | otherwise
  = show float
  where
    floatIsNeg = (float < 0) || isNegativeZero float

-- | Given a list of elements as arguments, show a @Vector@ expression. For
-- example, @'genVector' [\"27\", \"42\"]@ will return
-- @\"updateVector (updateVector newVector 0 27) 1 42)\"@.
genVector :: [String] -> String
genVector vals =
  snd $
  foldl'
    (\(!i, !v) x ->
      (i+1, "update (" ++ v ++ ") " ++ show i ++ " (" ++ x ++ ")"))
    (0 :: Int, "newVector")
    vals

-- | Display a value of a given type in Bluespec using @$display@.
class DisplayableInBluespec a where
  displayInBluespec ::
       proxy a  -- ^ The type of the value.
    -> String   -- ^ The name of the Bluespec variable.
    -> [String] -- ^ All arguments that are passed to @$display@.

-- | Most Bluespec types can be displayed using @fshow@.
fshowDisplay :: proxy a -> String -> [String]
fshowDisplay _ output = ["(fshow " ++ output ++ ")"]

-- | We display floating-point numbers by converting them to an integer and
-- printing them in hexadecimal (using the @%x@ modifier). This somewhat unusual
-- choice is motivated by the fact that this output is easier to parse than the
-- @fshow@ output.
hexFloatDisplay :: proxy a -> String -> [String]
hexFloatDisplay _ output = ["\"%x\"", output]

instance DisplayableInBluespec Bool where
  displayInBluespec = fshowDisplay

instance DisplayableInBluespec Int8 where
  displayInBluespec = fshowDisplay

instance DisplayableInBluespec Int16 where
  displayInBluespec = fshowDisplay

instance DisplayableInBluespec Int32 where
  displayInBluespec = fshowDisplay

instance DisplayableInBluespec Int64 where
  displayInBluespec = fshowDisplay

instance DisplayableInBluespec Word8 where
  displayInBluespec = fshowDisplay

instance DisplayableInBluespec Word16 where
  displayInBluespec = fshowDisplay

instance DisplayableInBluespec Word32 where
  displayInBluespec = fshowDisplay

instance DisplayableInBluespec Word64 where
  displayInBluespec = fshowDisplay

instance DisplayableInBluespec Float where
  displayInBluespec = hexFloatDisplay

instance DisplayableInBluespec Double where
  displayInBluespec = hexFloatDisplay

instance DisplayableInBluespec a => DisplayableInBluespec (Array n a) where
  displayInBluespec = fshowDisplay

-- | @copilot-bluespec@–generated structs currently do not have @FShow@
-- instances. (Perhaps they should: see
-- https://github.com/Copilot-Language/copilot-bluespec/issues/12)
-- In lieu of this, we manually define the same code that would be used in a
-- derived @FShow@ instance.
instance DisplayableInBluespec MyStruct where
  displayInBluespec _ output =
    [ "\"MyStruct { myStruct1 = %d ; myStruct2 = %d }\""
    , output ++ ".myStruct1"
    , output ++ ".myStruct2"
    ]

-- | Read a value of a given type in Bluespec.
class ReadableFromBluespec a where
  readFromBluespec :: String -> a

instance ReadableFromBluespec Bool where
  readFromBluespec = read

instance ReadableFromBluespec Int8 where
  readFromBluespec = read

instance ReadableFromBluespec Int16 where
  readFromBluespec = read

instance ReadableFromBluespec Int32 where
  readFromBluespec = read

instance ReadableFromBluespec Int64 where
  readFromBluespec = read

instance ReadableFromBluespec Word8 where
  readFromBluespec = read

instance ReadableFromBluespec Word16 where
  readFromBluespec = read

instance ReadableFromBluespec Word32 where
  readFromBluespec = read

instance ReadableFromBluespec Word64 where
  readFromBluespec = read

-- We print out floating-point values in hexadecimal (see `hexFloatDisplay`
-- above), so we parse them accordingly here.

instance ReadableFromBluespec Float where
  readFromBluespec s = castWord32ToFloat $ read $ "0x" ++ s

instance ReadableFromBluespec Double where
  readFromBluespec s = castWord64ToDouble $ read $ "0x" ++ s

instance (KnownNat n, ReadableFromBluespec a) => ReadableFromBluespec (Array n a) where
  readFromBluespec s0
    | Just s1 <- stripPrefix "<V" s0
    , Just s2 <- stripSuffix ">" s1
    = array $ map readFromBluespec $ words s2
    | otherwise
    = error $ "Unexpected Vector fshow output: " ++ s0

instance ReadableFromBluespec MyStruct where
  readFromBluespec str =
    case readsEither (readsStruct minPrec) str of
      Left err -> error err
      Right ms -> ms

-- | Attempt to read a value of type @a@. If successful, return 'Right' with
-- the read value. Otherwise, return @'Left' err@, where @err@ is an error
-- message describing what went wrong.
readsEither :: ReadS a -> String -> Either String a
readsEither readIt s =
  case [ x | (x,"") <- readIt s ] of
    [x] -> Right x
    []  -> Left $ "readsEither: no parse: " ++ s
    _   -> Left $ "readsEither: ambiguous parse: " ++ s

-- ** Orphan instances for Arrays

instance Eq a => Eq (Array n a) where
  a1 == a2 = arrayElems a1 == arrayElems a2

instance AEq a => AEq (Array n a) where
  a1 === a2 = arrayElems a1 === arrayElems a2

-- ** A simple struct definition for unit testing purposes

data MyStruct = MyStruct
  { myStruct1 :: Field "myStruct1" Int8
  , myStruct2 :: Field "myStruct2" Int8
  }

-- | Like a derived @Eq@ instance, except that this looks through 'Field's.
instance Eq MyStruct where
  MyStruct (Field f1a) (Field f2a) == MyStruct (Field f1b) (Field f2b) =
    f1a == f1b && f2a == f2b
instance AEq MyStruct

-- | Like a derived @Read@ instance, except that this adds 'Field' wrappers as
-- needed.
readsStruct :: Int -> ReadS MyStruct
readsStruct p = readParen (p > 10) $ \s -> do
  ("MyStruct", s1) <- lex s
  ("{", s2) <- lex s1
  ("myStruct1", s3) <- lex s2
  ("=", s4) <- lex s3
  (f1, s5) <- readsPrec 0 s4
  (";", s6) <- lex s5
  ("myStruct2", s7) <- lex s6
  ("=", s8) <- lex s7
  (f2, s9) <- readsPrec 0 s8
  ("}", s10) <- lex s9
  pure (MyStruct (Field f1) (Field f2), s10)

instance Struct MyStruct where
  typeName _ = "MyStruct"
  toValues ms = [ Value Int8 (myStruct1 ms)
                , Value Int8 (myStruct2 ms)
                ]

instance Typed MyStruct where
  typeOf = Struct (MyStruct (Field 0) (Field 0))

-- | Unwrap a 'Field' to obtain the underlying value.
unField :: Field s t -> t
unField (Field val) = val
