{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import Data.Foldable (for_)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import Data.Traversable (for)
import Options.Applicative

import Copilot.Verifier (Verbosity(..))
import Copilot.Verifier.Examples (shouldFailExamples, shouldPassExamples)

newtype Options = Options
  { examples :: [CI Text]
  } deriving Show

optsParser :: Parser Options
optsParser = Options
  <$> (some . strArgument)
      (  metavar "EXAMPLE1 [EXAMPLE2 ...]"
      <> help "The names of the examples to run" )

main :: IO ()
main = execParser opts >>= verifyExamples
  where
    opts = info (optsParser <**> helper)
      ( fullDesc
     <> header "Run one or more copilot-verifier examples"
     <> progDesc (unlines [ "Run one or more examples from the copilot-verifier/examples directory."
                          , "Each EXAMPLE must correspond to an example name."
                          ]))

verifyExamples :: Options -> IO ()
verifyExamples Options{examples} = do
  -- Check that all requested examples exist
  examplesWithMain <- for examples $ \example ->
    case Map.lookup example (shouldFailExamples Default `Map.union` shouldPassExamples Default) of
      Just m  -> pure (example, m)
      Nothing -> fail $ "No example named " ++ Text.unpack (CI.original example)

  -- Run the examples
  for_ examplesWithMain $ \(example, exampleMain) -> do
    Text.putStrLn "====="
    Text.putStrLn $ "== Running the " <> CI.original example <> " example..."
    Text.putStrLn "====="
    Text.putStrLn ""
    exampleMain
    Text.putStrLn ""
