

rotate_helper 0 (x:xs) = (x:xs)  
rotate_helper count (x:xs) = rotate_helper (count - 1) xs


add_rest 0 res _ = res
add_rest count res (x:xs) = add_rest (count-1) (res++[x]) xs
 

rotate x list = add_rest ((x) `mod` length(list)) (rotate_helper ((x) `mod` length(list)) list) list