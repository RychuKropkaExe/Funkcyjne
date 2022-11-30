helper_part res list [] = res
helper_part res list (x:xs) = helper_part (res++[[list++[x],xs]]) (list++[x]) xs

my_part list = helper_part [[[], list]] [] list


