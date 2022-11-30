import Data.List

helper_nub _ res [] = res 
helper_nub x res (y:ys) = if (compare (y) (x) == GT) then (helper_nub (y) (y:res) (ys)) else (helper_nub (x) (res) (ys))

my_nub [] = []
my_nub x = helper_nub (head sorted) [(head sorted)] sorted where sorted = sort x