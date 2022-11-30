data Tree a = Leaf a | Inner (Tree a) (Tree a)

instance (Eq a) => Eq (Tree a) where
    (Leaf a) == (Leaf b) = a == b
    (Inner a1 a2) == (Inner b1 b2) = a1 == b1 && a2 == b2  
    _ == _ = False

instance (Show a) => Show (Tree a) where
  show (Leaf x)     = show x
  show (Inner lt rt) =  "<" ++ show lt ++ "|" ++ show rt ++ ">"


instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Inner a  b) = Inner (fmap f a) (fmap f b)  

mypure = Leaf 
myreturn = Leaf

instance Applicative Tree where
    pure = mypure
    (Inner f1 f2) <*> (Inner a1 a2) = Inner (f1 <*> a1) (f2 <*> a2)
    (Leaf f) <*> (Leaf a) = Leaf (f a)
    -- _ <*> _ = Nothing 
-- *Main> a = Leaf 5 
-- *Main> b = Leaf ((*) 5)
-- *Main> a <*> b

-- (Applicative) :

-- *Main> a <*> b 
-- (7,7)
-- *Main> d = Inner (Inner b b) (Inner b b)
-- *Main> c = Inner (Inner a a) (Inner a a)
-- *Main> d <*> c
-- <<25|25>|<25|25>>


-- Tożsamość (pure id <*> x = x) : 

-- *Main> mypure id <*> a 
-- 5

-- Tożsamość (pure f <*> pure x = pure (f x)) :

-- *Main> mypure ((*) 5) <*> mypure 5 
-- 25

-- Tożsamość (x <*> pure y = pure ($ y) <*> x):

-- *Main> b <*> pure 5
-- 25
-- *Main> pure ($ 5) <*> b
-- 25

-- Tożsamość (pure (.) <*> x <*> y <*> z = x <*> (y <*> z)) :

-- *Main> b <*> (b2 <*> a)
-- 160

-- *Main> pure (.) <*> b <*> b2 <*> a
-- 160


instance Monad Tree where
    return = myreturn
    (Leaf x) >>= f = f x
    (Inner a b) >>= f = Inner (a >>= f) (b >>= f)

-- Tożsamość ((return x) >>= f ==== f x) :

-- *Main> (return 5) >>= f 
-- 50
-- *Main> f 5 
-- 50

-- Tożsamość x >>= return ==== x

-- *Main> a >>= myreturn
-- 5

-- Tożsamość (x >>= f) >>= g ==== x >>= (\y -> f y >>= g)

-- *Main> g a = Leaf (a^2)
-- *Main> (a >>= f) >>= g
-- 2500

-- *Main> a >>= (\y -> f y >>= g)
-- 2500