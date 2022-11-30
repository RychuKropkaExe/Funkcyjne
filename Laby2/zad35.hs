my_permutations [] = [[]]
my_permutations xs = [ y : ps | (y,ys) <- selections xs, ps <- my_permutations ys]

selections []     = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]