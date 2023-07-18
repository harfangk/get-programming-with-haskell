main = print ""

ifEven f n =
  if even n
    then f n
    else n

inc = (+) 1

double = (*) 2

square = (^) 2

ifEvenInc = ifEven inc

ifEvenDouble = ifEven double

ifEvenSquare = ifEven square

binaryPartialApplication f x = (\y -> f x y)

binaryPartial = binaryPartialApplication (+) 1
