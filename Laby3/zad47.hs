factorial 0 = 1
factorial a = a * factorial (a - 1)


aprrox_l n = foldl (\acc k -> acc + ( 1 / (factorial k)) ) 0.0 [1..n]
aprrox_r n = foldr (\k acc -> acc + ( 1 / (factorial k)) ) 0.0 [1..n]