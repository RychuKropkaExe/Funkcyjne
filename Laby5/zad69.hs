import Data.Semigroup

data Pkt a = Pkt (a,a)

instance (Show a) => Show (Pkt a) where
    show (Pkt (a1,a2)) = show (a1,a2)

instance Functor Pkt where
    fmap f (Pkt (a1,a2)) = Pkt (f a1, f a2)


myreturn a = Pkt (a, a)
mypure a = Pkt (a, a)

instance Applicative Pkt where 
  pure = mypure
  (Pkt (f,g)) <*> (Pkt (a,b)) = Pkt ((f a),(g b))

-- *Main> f = (+) 2
-- *Main> g = (+) 5
-- *Main> a = Pkt (f,g)
-- *Main> b = Pkt (5,2)

-- (Applicative) :

-- *Main> a <*> b 
-- (7,7)

-- Tożsamość (pure id <*> x = x) : 

-- *Main> f a = a
-- *Main> g a = a
-- *Main> p = Pkt (f,g)
-- *Main> p <*> b
-- (5,2)
-- *Main> mypure id <*> b 
-- (5,2)

-- Tożsamość (pure f <*> pure x = pure (f x)) :

-- *Main> mypure ((+) 5) <*> mypure 5   
-- (10,10)
-- *Main> mypure ((+) 5 5)
-- (10,10)

-- Tożsamość (x <*> pure y = pure (y) <*> x):

-- *Main> p <*> mypure 5
-- (5,5)
-- *Main> mypure ($ 5) <*> p 
-- (5,5)

-- Tożsamość (pure (.) <*> x <*> y <*> z = x <*> (y <*> z)) :

-- *Main> p <*> ( Pkt( (+) 2, (*) 5) <*> b)
-- (7,10)
-- *Main> p <*> Pkt( (+) 2, (*) 5) <*> b
-- (7,10)
-- *Main> mypure (.) <*> p <*> Pkt( (+) 2, (*) 5) <*> b
-- (7,10)

instance Monad Pkt where
    return = myreturn
    (Pkt (a1,a2)) >>= f = Pkt (x1,y2) where
         Pkt (x1,y1) = f a1
         Pkt (x2,y2) = f a2


-- Tożsamość ((return x) >>= f ==== f x) :

-- *Main> f x = Pkt ((x*10),(x+12))

-- *Main> (myreturn 5) >>= f
-- (50,17)

-- Tożsamość x >>= return ==== x

-- *Main> Pkt (5,5) >>= myreturn 
-- (5,5)

-- Tożsamość (x >>= f) >>= g ==== x >>= (\y -> f y >>= g)

-- *Main> a = Pkt (5,2)
-- g x = Pkt ((x-2), (x/10))
-- (a >>= f) >>= g 
-- (48.0,1.4)
-- *Main> a >>= (\y -> f y >>= g)
-- (48.0,1.4)

--- SEQUENCE
-- *Main Data.Sequence> sequence [a, a ,a ]
-- ([5,5,5],[2,2,2])
-- *Main Data.Sequence> sequence [a, a, a]
-- ([5,5,5],[2,2,2])
-- *Main Data.Sequence> b = Pkt (1,2)
-- *Main Data.Sequence> c = Pkt (2,3)
-- *Main Data.Sequence> sequence [a,b,c]
-- ([5,1,2],[2,2,3])