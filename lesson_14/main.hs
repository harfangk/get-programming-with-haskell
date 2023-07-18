data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show, Enum)

instance Ord SixSidedDie where
  compare d1 d2 = compare (fromEnum d1) (fromEnum d2)

instance Eq SixSidedDie where
  (==) d1 d2 = (==) (fromEnum d1) (fromEnum d2)

data FiveSidedDie = FS1 | FS2 | FS3 | FS4 | FS5 deriving (Show, Eq, Ord)

instance Die FiveSidedDie where
  min = FS1
  max = FS5

class Die a where
  min :: a
  max :: a
