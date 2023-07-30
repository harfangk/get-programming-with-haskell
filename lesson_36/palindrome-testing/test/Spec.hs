import Data.Char (isPunctuation)
import Data.Text as T
import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances

main :: IO ()
main = do
  quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant
  putStrLn "Done!"

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement =
  if test
    then putStrLn passStatement
    else putStrLn failStatement

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
  where
    noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)
