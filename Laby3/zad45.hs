remdup [] = []
remdup (x:xs) = snd (foldl (\(n, acc) y -> if (n /= y) then (y, acc++[y]) else (n,acc)) (x, [x]) (xs))

