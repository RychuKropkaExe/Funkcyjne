helper_non_dec x [] = True
helper_non_dec x (y:ys) = if compare x y == GT then False else (helper_non_dec y ys)

non_dec list = helper_non_dec (head list) list