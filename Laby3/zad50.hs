

helper_take _ [] _ = []
helper_take f (x:xs) res = if f x then (helper_take f xs (res++[x]) ) else res

myTakeWhile f list = helper_take f list []  

myDropWhile _ [] = []
myDropWhile f (x:xs) = if f x then myDropWhile f xs else (x:xs)