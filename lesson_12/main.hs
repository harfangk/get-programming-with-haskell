main = print ""

patientInfo :: PatientName -> Int -> Int -> String
patientInfo patientName age height = name ++ " " ++ ageHeight
  where
    name = lastName patientName ++ ", " ++ firstName patientName
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type PatientName = (String, String)

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }

type FirstName = String

type LastName = String

data RhType = Pos | Neg

data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

data Sex = Male | Female

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

showHeight :: Int -> String
showHeight h = show h ++ " in."

showWeight :: Int -> String
showWeight w = show w ++ " lbs."

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

canDonateToPatient :: Patient -> Patient -> Bool
canDonateToPatient p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

jackieSmith :: Patient
jackieSmith =
  Patient
    { name = Name "Jackie" "Smith",
      age = 43,
      sex = Female,
      height = 62,
      weight = 115,
      bloodType = BloodType O Neg
    }

patientSummary :: Patient -> String
patientSummary p =
  "**************"
    ++ "\n"
    ++ "Patient Name: "
    ++ showName (name p)
    ++ "\n"
    ++ "Sex: "
    ++ showSex (sex p)
    ++ "\n"
    ++ "Age: "
    ++ show (age p)
    ++ "\n"
    ++ "Height: "
    ++ showHeight (height p)
    ++ "\n"
    ++ "Weight: "
    ++ showWeight (weight p)
    ++ "\n"
    ++ "Blood Type: "
    ++ showBloodType (bloodType p)
    ++ "\n"
    ++ "**************"
