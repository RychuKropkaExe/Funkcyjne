helper_tails res [] = res
helper_tails res x = helper_tails (x:res) (tail x)  

my_tails [] = []
my_tails list = []:(helper_tails [] list)