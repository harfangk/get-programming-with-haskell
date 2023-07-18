import Data.Char (toLower)

main = print ""

remove test [] = []
remove test (x : xs) =
  if test x
    then remove test xs
    else x : remove test xs

myProduct nums = foldl (*) 0 nums

elem _ [] = False
elem x list = not (any (x ==) list)

isPalindrome s = s == reverse s
  where
    filteredS = filter (/= ' ') s
    downcasedS = map toLower filteredS

harmonic n = sum (take n (map (1 /) [1 ..]))
