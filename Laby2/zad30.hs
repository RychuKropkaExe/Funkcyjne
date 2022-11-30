helper_inits res x [] = res++[x]
helper_inits res x (y:ys) = helper_inits (res++[x]) (x++[y]) ys

my_inits [] = []
my_inits list = helper_inits [] [] list 