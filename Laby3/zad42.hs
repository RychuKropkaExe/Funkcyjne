

helper [] = True
helper ((x,y):xs) = if x > y then False else helper xs
nondec x = helper (zip x (tail x))