my_log 0 y = y - 1 
my_log x y = my_log (x `div` 5) y+1


division_list x = [z | z <- [x `div` (5^y) | y<-[1..(my_log x 0)]]]

each_value list = foldl (-) (head list) (tail list)

helper_trail res _ [] = res
helper_trail res count (x:xs) = helper_trail (res+count*(each_value (x:xs))) (count+1) xs

trail_zeros x = helper_trail 0 1 (division_list x)


