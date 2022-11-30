

number_of_evens xs = foldr (\x acc -> acc + ((( x `mod` 2) + 1) `mod` 2)) 0 xs