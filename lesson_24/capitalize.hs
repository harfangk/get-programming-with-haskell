import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- TIO.readFile fileName
  TIO.appendFile ("capital_" <> fileName) (T.toUpper file)
