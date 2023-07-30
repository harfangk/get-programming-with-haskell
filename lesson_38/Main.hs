import Data.Char (isDigit)

main :: IO ()
main = do
  print "Enter a number to test for primality:"
  n <- read <$> getLine
  let result = isPrime n
  print (displayResult result)

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n - 1) (tail xs)

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM n (x : xs) = x : myTakePM (n - 1) xs

isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n > maxN = Left TooLarge
  | otherwise = Right (n `elem` primes)

maxN = 10000

primes :: [Int]
primes = sieve [2 .. maxN]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime : rest) = nextPrime : sieve noFactors
  where
    noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge = "Value exceed max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError

addStrInts :: String -> String -> Either String Int
addStrInts s1 s2
  | not (all isDigit s1) && not (all isDigit s2) = Left "Neither value can be parsed."
  | not (all isDigit s1) = Left "First value can't be parsed."
  | not (all isDigit s2) = Left "Second value can't be parsed."
  | otherwise = Right (read s1 + read s2)

safeSucc :: (Eq a, Bounded a, Enum a) => a -> Maybe a
safeSucc n
  | n == maxBound = Nothing
  | otherwise = Just (succ n)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x : xs) = xs

safeLast :: [a] -> Either String a
safeLast [] = Left "Empty list"
safeLast xs = safeLast' 10000 xs

safeLast' :: Int -> [a] -> Either String a
safeLast' 0 _ = Left "List exceeds safe bound"
safeLast' _ (x : []) = Right x
safeLast' n (x : xs) = safeLast' (n - 1) xs
