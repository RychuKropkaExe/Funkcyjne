my_zip _ [] = []
my_zip [] _ = []
my_zip (x:xs) (y:ys) = (x,y):my_zip xs ys 