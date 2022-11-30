import Data.Map

data Term =   X String      -- zmienna
            | N Float       -- stała
            | A Term Term   -- add
            | S Term Term   -- substract
            | M Term Term   -- multiply
            | D Term Term   -- divide
            | E Term -- exp
            | L Term -- ln

            

zmienne :: Map String Float
zmienne = fromList [("x", 5.2), ("y", 19.0), ("z", 22.3)]

termA = D (A (X "x")(X "y") ) (M (N 15) (X "z"))
termB = D (A (X "x")(N 10 ) ) (M (X "z") (N (-10)))
termC = D (N 10) (N 0)

eval:: Map String Float -> Term -> Maybe Float
eval z (X x) = Data.Map.lookup x z 
eval z (N x) = Just x 
eval z (A a b) = ((eval z a) >>= (\x -> Just ((+) x)))  <*> (eval z b)
eval z (S a b) = ((eval z a) >>= (\x -> Just ((-) x)))  <*> (eval z b)
eval z (M a b) = ((eval z a) >>= (\x -> Just ((*) x)))  <*> (eval z b)
eval z (D a b) = ((eval z a) >>= (\x -> Just ((/) x)))  <*> (eval z b)
eval z (E a) = (eval z a) >>= (\x -> Just (exp x))
eval z (L a) = (eval z a) >>= (\x -> Just (log x))

