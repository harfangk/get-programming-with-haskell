main = print ""

myTail [] = []
myTail (_ : xs) = xs

myGCD m n =
  case remainder of
    0 -> n
    rem -> myGCD n rem
  where
    remainder = m `mod` n
