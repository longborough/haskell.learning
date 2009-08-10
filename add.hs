add a b = a + b

myNot True  = False
myNot False = True

sumList []     = 0
sumList (x:xs) = x + sumList xs
