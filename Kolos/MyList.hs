-- Implementcja listy jednokierunkowej

data MyList a = Empty | MyList a (MyList a)

instance (Show a, Eq a) => Show (MyList a) where
    show Empty = "[]"
    show (MyList a b) = if b /= Empty then (show a) ++ ":" ++ (show b)
                        else (show a)

instance (Eq a) => Eq (MyList a) where
    Empty == Empty = True
    (MyList a1 b1) == (MyList a2 b2) = (a1 == a2) && (b1 == b2)
    _ == _ = False


myHead::(MyList a)->a
myHead Empty = error "Empty list"
myHead (MyList a _) = a 
myTail (MyList _ b) = b
myTail Empty = Empty
myLength Empty = 0
myLength (MyList a b) = 1 + (myLength b) 

instance Foldable MyList where
    foldl f e Empty = e  
    foldl f e (MyList a b) = foldl f (f e a) b
    foldr f e Empty = e    
    foldr f e (MyList a b) = f a (foldr f e b)
