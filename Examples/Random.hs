import Copilot.Core.PrettyPrint as P
import Copilot.Core.Random (random)
import Copilot.Core.Random.Weights
import Copilot.Language
import Copilot.Language.Prelude
import qualified Prelude as P
import System.Random (newStdGen)

main :: IO ()
main =
  do
    g <- newStdGen
    let p = random simpleWeights g
    putStrLn (P.prettyPrint p)
