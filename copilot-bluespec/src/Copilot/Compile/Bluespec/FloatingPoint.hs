-- | Generate definitions that allow importing C implementations of
-- floating-point operations into Bluespec.
module Copilot.Compile.Bluespec.FloatingPoint
  ( copilotBluespecFloatingPointBSV
  , copilotBluespecFloatingPointC
  ) where

-- | The contents of the generated @BluespecFP.bsv@ file, which contains the
-- @import \"BDPI\"@ declarations needed to use imported C functions in
-- Bluespec.
copilotBluespecFloatingPointBSV :: String
copilotBluespecFloatingPointBSV =
  unlines $
    [ "import FloatingPoint::*;"
    , ""
    ] ++
    concatMap
      (\funName -> [importOp1 Float funName, importOp1 Double funName])
      unaryFloatOpNames ++
    concatMap
      (\funName -> [importOp2 Float funName, importOp2 Double funName])
      [ "pow"
      , "atan2"
      , "logb"
      ]
  where
    importOp1 :: FloatType -> String -> String
    importOp1 ft funName =
      "import \"BDPI\" function " ++ floatTypeName ft ++ " " ++ funNamePrefix
        ++ funName ++ floatTypeSuffix ft ++ " (" ++ floatTypeName ft ++ " x);"

    importOp2 :: FloatType -> String -> String
    importOp2 ft funName =
      "import \"BDPI\" function " ++ floatTypeName ft ++ " " ++ funNamePrefix
        ++ funName ++ floatTypeSuffix ft ++ " (" ++ floatTypeName ft ++ " x, "
        ++ floatTypeName ft ++ " y);"

    floatTypeName :: FloatType -> String
    floatTypeName Float  = "Float"
    floatTypeName Double = "Double"

-- | The contents of the generated @bs_fp.c@ file, which contains the C wrapper
-- functions that Bluespec imports.
copilotBluespecFloatingPointC :: String
copilotBluespecFloatingPointC =
  unlines $
    [ "#include <math.h>"
    , ""
    , defineUnionType Float
    , defineUnionType Double
    ] ++
    concatMap
      (\funName -> [defineOp1 Float funName, defineOp1 Double funName])
      unaryFloatOpNames ++
    concatMap
      (\funName -> [defineOp2 Float funName, defineOp2 Double funName])
      [ "pow"
      , "atan2"
      ] ++
    -- There is no direct C counterpart to the `logb` function, so we implement
    -- it in terms of `log`.
    map
      (\ft ->
        defineOp2Skeleton ft "logb" $
        \_cFunName x y -> "log" ++ floatTypeSuffix ft ++ "(" ++ y ++ ") / log"
                            ++ floatTypeSuffix ft ++ "( " ++ x ++ ")")
      [Float, Double]
  where
    defineUnionType :: FloatType -> String
    defineUnionType ft =
      unlines
        [ "union " ++ unionTypeName ft ++ " {"
        , "  " ++ integerTypeName ft ++ " i;"
        , "  " ++ floatTypeName ft ++ " f;"
        , "};"
        ]

    defineOp1Skeleton ::
      FloatType -> String -> (String -> String -> String) -> String
    defineOp1Skeleton ft funName mkFloatOp =
      let cFunName = funName ++ floatTypeSuffix ft in
      unlines
        [ integerTypeName ft ++ " "
          ++ funNamePrefix ++ cFunName
          ++ "(" ++ integerTypeName ft ++ " x) {"
        , "  union " ++ unionTypeName ft ++ " x_u;"
        , "  union " ++ unionTypeName ft ++ " r_u;"
        , "  x_u.i = x;"
        , "  r_u.f = " ++ mkFloatOp cFunName "x_u.f" ++ ";"
        , "  return r_u.i;"
        , "}"
        ]

    defineOp1 :: FloatType -> String -> String
    defineOp1 ft funName =
      defineOp1Skeleton ft funName $
      \cFunName x -> cFunName ++ "(" ++ x ++ ")"

    defineOp2Skeleton ::
      FloatType -> String -> (String -> String -> String -> String) -> String
    defineOp2Skeleton ft funName mkFloatOp =
      let cFunName = funName ++ floatTypeSuffix ft in
      unlines
        [ integerTypeName ft ++ " "
          ++ funNamePrefix ++ cFunName
          ++ "(" ++ integerTypeName ft ++ " x, " ++ integerTypeName ft
          ++ " y) {"
        , "  union " ++ unionTypeName ft ++ " x_u;"
        , "  union " ++ unionTypeName ft ++ " y_u;"
        , "  union " ++ unionTypeName ft ++ " r_u;"
        , "  x_u.i = x;"
        , "  y_u.i = y;"
        , "  r_u.f = " ++ mkFloatOp cFunName "x_u.f" "y_u.f" ++ ";"
        , "  return r_u.i;"
        , "}"
        ]

    defineOp2 :: FloatType -> String -> String
    defineOp2 ft funName =
      defineOp2Skeleton ft funName $
      \cFunName x y -> cFunName ++ "(" ++ x ++ ", " ++ y ++ ")"

    integerTypeName :: FloatType -> String
    integerTypeName Float  = "unsigned int"
    integerTypeName Double = "unsigned long long"

    floatTypeName :: FloatType -> String
    floatTypeName Float  = "float"
    floatTypeName Double = "double"

    unionTypeName :: FloatType -> String
    unionTypeName Float  = "ui_float"
    unionTypeName Double = "ull_double"

-- * Internals

-- | Are we generating a function for a @float@ or @double@ operation?
data FloatType
  = Float
  | Double

-- | The suffix to use in the generated function.
floatTypeSuffix :: FloatType -> String
floatTypeSuffix Float  = "f"
floatTypeSuffix Double = ""

-- | The prefix to use in the generated function.
funNamePrefix :: String
funNamePrefix = "bs_fp_"

-- | The names of unary floating-point operations.
unaryFloatOpNames :: [String]
unaryFloatOpNames =
  [ "exp"
  , "log"
  , "acos"
  , "asin"
  , "atan"
  , "cos"
  , "sin"
  , "tan"
  , "acosh"
  , "asinh"
  , "atanh"
  , "cosh"
  , "sinh"
  , "tanh"
  , "ceil"
  , "floor"
  , "sqrt"
  ]
