import Distribution.Simple
import System.Exit (exitWith, ExitCode(..))

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{ postInst = msg }
  where 
  msg _ _ _ _ = do
    putStrLn "Execute \"<Your install location>/.cabal/bin/copilot-regression\" to run some very simple tests to ensure your installation built correctly."
    putStrLn "To QuickCheck the Atom backend, execute \"<Your install location>/.cabal/bin/copilot-c99-qc .\""
    exitWith ExitSuccess

