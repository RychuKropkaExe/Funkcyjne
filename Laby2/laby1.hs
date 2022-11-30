variance x = foldr (\x (n,s,ss) -> (n+1,s+x,ss+(x^2))) (0,0,0) x
count (n,s,ss) = (ss/n) - (s/n)^2
