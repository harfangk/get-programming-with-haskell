import Data.Map qualified as Map

main :: IO ()
main =
  putStrLn "What is the size of pizza 1"
    >> getLine
    >>= ( \size1 ->
            putStrLn "What is the cost of pizza 1"
              >> getLine
              >>= ( \cost1 ->
                      putStrLn "What is the size of pizza 2"
                        >> getLine
                        >>= ( \size2 ->
                                putStrLn "What is the cost of pizza 2"
                                  >> getLine
                                  >>= (\cost2 -> putStrLn (describePizza (comparePizzas (read size1, read cost1) (read size2, read cost2))))
                            )
                  )
        )

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if costP1 < costP2
    then p1
    else p2
  where
    costP1 = costPerInch p1
    costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size, cost) = "The " ++ show size ++ " pizza " ++ "is cheaper at " ++ show costSqInch ++ " per square inch"
  where
    costSqInch = costPerInch (size, cost)

type Pizza = (Double, Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2) ^ 2

costData :: Map.Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

listMain :: [String]
listMain = do
  let pizzas = zip (map snd . Map.toList $ sizeData) (map snd . Map.toList $ costData)
  let betterPizza = foldl1 comparePizzas pizzas
  return (describePizza betterPizza)

monadMain :: (Monad m) => m String
monadMain = do
  let pizzas = zip (map snd . Map.toList $ sizeData) (map snd . Map.toList $ costData)
  let betterPizza = foldl1 comparePizzas pizzas
  return (describePizza betterPizza)
