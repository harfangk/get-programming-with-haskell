main = print ""

myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y counter = fastFib (x + y) x (counter - 1)
