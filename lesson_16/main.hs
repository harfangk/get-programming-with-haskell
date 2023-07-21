data StoreItem
  = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet

data CollectibleToy = CollectibleToy
  { name :: String,
    descrption :: String,
    toyPrice :: Double
  }

data VinylRecord = VinylRecord
  { artist ::
      Creator,
    recordTitle ::
      String,
    recordYear ::
      Int,
    recordPrice ::
      Double
  }

data Book = Book
  { author ::
      Creator,
    isbn ::
      String,
    bookTitle :: String,
    bookYear :: Int,
    bookPrice :: Double
  }

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char

type FirstName = String

type LastName = String

type MiddleName = String

data Creator = AuthorCreator Author | ArtistCreator Artist

data Author = Author Name

data Artist = Person Name | Band String

data Pamphlet = Pamplet
  { title :: String,
    description :: String,
    contact :: String
  }

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = 0

data Shape
  = Circle Double
  | Square Double
  | Rectangle Double Double

area :: Shape -> Double
area (Circle radius) = radius * radius * pi
area (Rectangle width height) = width * height
area (Square side) = side * side

perimeter :: Shape -> Double
perimeter (Circle radius) = 2 * radius * pi
perimeter (Rectangle width height) = 2 * width + 2 * height
perimeter (Square side) = 4 * side
