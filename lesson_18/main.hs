import Data.Map qualified as Map

data Triple a = Triple a a a deriving (Show)

newtype Box a = Box a deriving (Show)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

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
