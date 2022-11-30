data RF2 = RF2 {a::Float , b::Float}
-- 1.
class VectorSpace a where
    vnull::a
    vadd, vmult::a->a->a
    isBasis::[a] -> Bool
    (<.>)::a->a->Float
    vadd x vnull = x 

-- a1b2 - b1a2
instance Show RF2 where
    show (RF2 a b) = show (a,b)

-- 2.
instance VectorSpace RF2 where
    vnull = RF2 0 0
    vadd a1 a2 = RF2 ((a a1) + (a a2))  ((b a1) + (b a2)) 
    vmult a1 a2 = RF2 ((a a1) * (a a2))  ((b a1) * (b a2)) 
    isBasis [x1,x2] = ((a x1)*(b x2) - (b x1)*(a x2)) /= 0  
    isBasis _ = False
    (<.>) a1 a2 = ((a a1)*(a a2)) + ((b a1)*(b a2)) 

