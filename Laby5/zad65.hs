data Tree a = Leaf a | Inner (Tree a) (Tree a)

instance (Eq a) => Eq (Tree a) where
    (Leaf a) == (Leaf b) = a == b
    (Inner a1 a2) == (Inner b1 b2) = a1 == b1 && a2 == b2  
    _ == _ = False

instance (Show a) => Show (Tree a) where
  show (Leaf x)     = show x
  show (Inner lt rt) =  "<" ++ show lt ++ "|" ++ show rt ++ ">"

connect :: Tree a -> Tree a -> Tree a
connect a b = Inner a b

treeDepth (Leaf _) = 0 
treeDepth (Inner a b) = (max (treeDepth a) (treeDepth b)) + 1

treeB::Int->Tree Int
treeB 0 = Leaf 1
treeB n = Inner (Leaf 0) (treeB (n-1)) 

-- treeCon::Int->(Tree Char)
treeCon 0 = Leaf "a"
treeCon n = Inner (Leaf "b") (treeCon (n-1))

treeReplace::Tree a ->(a -> Tree b)->Tree b
treeReplace (Leaf a) f = f a
treeReplace (Inner a b) f = Inner (treeReplace a f) (treeReplace b f) 
    -- let mytree = Inner (Inner (Leaf 1) (Leaf 2)) (Leaf 6)
    -- let mytree2 = Inner (Inner (Leaf 1) (Leaf 2)) (Leaf 5)
    -- let mytree3 = connect mytree mytree2
    -- show mytree3