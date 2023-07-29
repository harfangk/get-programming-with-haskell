import Data.Text qualified as T
import Data.Text.IO qualified as TI
import System.Environment
import System.IO

getCounts :: String -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = length input
    wordCount = (length . words) input
    lineCount = (length . lines) input

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) =
  unwords
    [ "chars: ",
      show cc,
      " words: ",
      show wc,
      " lines: ",
      show lc
    ]

main :: IO ()
main = do
  filePaths <- getArgs
  let originalFilePath = head filePaths
  let copiedFilePath = head . tail $ filePaths
  originalFile <- TI.readFile originalFilePath
  TI.appendFile copiedFilePath originalFile
