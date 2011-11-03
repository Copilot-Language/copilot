import Copilot.Core.PrettyPrint as P
import Copilot.Core.Random (randomSpec)
import Copilot.Core.Random.Weights (simpleWeights)
import Copilot.Language
import Copilot.Language.Prelude
import qualified Prelude as P
import System.Random (newStdGen)

main :: IO ()
main = do
  g <- newStdGen
  let p = randomSpec simpleWeights g
  putStrLn (P.prettyPrint p)
