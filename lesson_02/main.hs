main =
  print ""

inc = (+) 1

double = (*) 2

square = (**) 2

change n =
  if even n
    then n - 2
    else n * 3 + 1
