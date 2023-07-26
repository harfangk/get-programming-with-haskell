import Data.Map qualified as Map

numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just n) = n

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organInventory :: Map.Map Organ Int
organInventory = foldl f Map.empty organs
  where
    inc Nothing = Just 1
    inc (Just n) = Just (n + 1)
    f acc o = Map.alter inc o acc

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where
    getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

emptyDrawers :: Int
emptyDrawers = length $ filter (\x -> x == Nothing) availableOrgans

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving (Show)

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container) = show container ++ " in the " ++ show location

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)
