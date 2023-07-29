import Data.List qualified

allFmap :: (Applicative f) => (a -> b) -> f a -> f b
allFmap func fa = pure func <*> fa

example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6

lastNightPurchase = [6, 12]

drankLastNight = 4

friends = [2, 3]

drinksPerPerson = [3, 4]

neededBeer = Data.List.maximum (((-) <$> ((*) <$> friends <*> drinksPerPerson)) <*> (flip (-) 4 <$> lastNightPurchase))
