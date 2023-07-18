main = print ""

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x : xs) =
  if f x
    then x : filter xs
    else filter xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f newInit xs
  where
    newInit = f init x
