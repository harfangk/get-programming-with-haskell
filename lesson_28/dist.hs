import Data.Map qualified as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB =
  Map.fromList
    [ ("Arkham", (42.6054, -70.7829)),
      ("Innsmouth", (42.8250, -70.8150)),
      ("Carcosa", (29.9714, -90.7694)),
      ("New York", (40.7776, -73.9691))
    ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
  where
    rlat = toRadians lat
    rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where
    (rlat1, rlong1) = latLongToRads coords1
    (rlat2, rlong2) = latLongToRads coords2
    dlat = rlat2 - rlat1
    dlong = rlong2 - rlong1
    a = (sin (dlat / 2)) ^ 2 + cos rlat1 * cos rlat2 * (sin (dlong / 2)) ^ 2
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe x y = (+) <$> x <*> y

main :: IO ()
main = do
  putStrLn "Enter the starting city name:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Enter the destination city name:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO latlong1 latlong2 = haversine <$> latlong1 <*> latlong2

robots :: IO ()
robots = do
  putStrLn "Enter the ID of the first part:"
  firstPartId <- getLine
  let firstPart = Map.lookup (read firstPartId) partsDB
  putStrLn "Enter the ID of the second part: "
  secondPartId <- getLine
  let secondPart = Map.lookup (read secondPartId) partsDB
  let cheaperPart = getCheaperRobot <$> firstPart <*> secondPart
  case cheaperPart of
    Nothing -> putStrLn "One of the parts not found"
    Just r -> print r

getCheaperRobot :: RobotPart -> RobotPart -> RobotPart
getCheaperRobot r1 r2 =
  if cost r1 <= cost r2
    then r1
    else r2

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3, 4, 5]
    vals = [leftArm, rightArm, robotHead, leftLeg, rightLeg]
    keyVals = zip keys vals

data RobotPart = RobotPart
  { name :: String,
    description :: String,
    cost :: Double,
    count :: Int
  }
  deriving (Show)

leftArm :: RobotPart
leftArm =
  RobotPart
    { name = "left arm",
      description = "left arm for face punching!",
      cost = 1000.00,
      count = 3
    }

rightArm :: RobotPart
rightArm =
  RobotPart
    { name = "right arm",
      description = "right arm for kind hand gestures",
      cost = 1025.00,
      count = 5
    }

robotHead :: RobotPart
robotHead =
  RobotPart
    { name = "robot head",
      description = "this head looks mad",
      cost = 5092.25,
      count = 2
    }

leftLeg :: RobotPart
leftLeg =
  RobotPart
    { name = "left leg",
      description = "powerful left leg",
      cost = 192.25,
      count = 4
    }

rightLeg :: RobotPart
rightLeg =
  RobotPart
    { name = "right leg",
      description = "weak right leg",
      cost = 2192.25,
      count = 1
    }
