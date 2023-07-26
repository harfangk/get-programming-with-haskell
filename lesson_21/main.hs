helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
  name <- getLine
  let statement = helloPerson name
  putStrLn statement

getFib :: IO ()
getFib = do
  target <- getLine
  let result = fastFib 2 1 (read target)
  print result

fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y counter = fastFib (x + y) x (counter - 1)
