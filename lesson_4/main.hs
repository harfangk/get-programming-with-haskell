main = print ""

compareLastNames name1 name2 =
  case compare lastName1 lastName2 of
    GT -> GT
    LT -> LT
    EQ -> compare firstName1 firstName2
  where
    lastName1 = snd name1
    lastName2 = snd name2
    firstName1 = fst name1
    firstName2 = fst name2

addressLetter name location = locationFunction name
  where
    locationFunction = getLocationFunction location

getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" -> dcOffice
  _ -> (\name -> (fst name) ++ " " ++ (snd name))

sfOffice name =
  if lastName < "L"
    then
      nameText
        ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else
      nameText
        ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = snd name

dcOffice name = nameText ++ " - PO Box 456 - Washington DC 89523"
  where
    nameText = "Esq." ++ snd name
