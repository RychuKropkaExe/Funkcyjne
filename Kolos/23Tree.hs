import Control.Monad.Writer
import Data.Semigroup 
--Mamy liść, węzeł binarny, lub węzeł potrójny
data MyTree a = Leaf a | BinaryBranch a (MyTree a) (MyTree a) | TerenaryBranch a (MyTree a) (MyTree a) (MyTree a) deriving(Eq)

-- Przykładowe drzewa do testów
tree1 = Leaf 10
tree2 = BinaryBranch 15 (Leaf 10) (Leaf 5)
tree3 = TerenaryBranch 20 (Leaf 5) (Leaf 8) (Leaf 9)
tree4 = TerenaryBranch 10 (Leaf 3) (BinaryBranch 3 (Leaf 2) (Leaf 3)) (Leaf 10)
tree5 = BinaryBranch 10 tree2 tree3
tree6 = TerenaryBranch 20 tree3 tree4 tree5

--Jako taki show, dla małych instancji jest w miare czytelne
--{a} to jest Liść, (...) to jest binarny branch, [...] To jest potrójny branch
instance (Show a) => Show (MyTree a) where
    show (Leaf a) = "{" ++ (show a) ++ "}"  
    show (BinaryBranch a b c) = "(" ++ (show b) ++ "/ " ++ (show a) ++ " \\" ++ (show c) ++ ")"
    show (TerenaryBranch a b c d) = "[" ++ (show b) ++ "/ " ++ (show a) ++ " |" ++ (show c) ++ " \\" ++ (show d) ++ "]"

--Funktor, tutaj nie ma za bardzo co się rozwodzić, mamy fmap i tyle.
instance Functor MyTree where
    fmap f (Leaf a) = Leaf (f a)  
    fmap f (BinaryBranch a b c) = BinaryBranch (f a) (fmap f b) (fmap f c)
    fmap f (TerenaryBranch a b c d) = TerenaryBranch (f a) (fmap f b) (fmap f c) (fmap f d)

--Tożsamości:
--Identyczność:
-- *Main> fmap id tree2
-- ({10} / 15\{5})
--Łączność:
-- *Main> f = (+) 5
-- *Main> g = (*) 10
-- *Main> fmap (f.g) tree2
-- ({105}/ 155 \{55})
-- *Main> c = fmap f. fmap g
-- *Main> c tree2
-- ({105}/ 155 \{55})

--Expand, nic szczególnego, po prostu Przechodzimy po całym drzewie, a jak trafimy
--Na binarny węzeł, to zamieniamy go na potrójny, zwracamy przekształcone drzewo
expand::(MyTree a)->a->(MyTree a)
expand (Leaf a) _ = (Leaf a)
expand (BinaryBranch a b c) x = TerenaryBranch a (Leaf x) (expand b x) (expand c x)
expand (TerenaryBranch a b c d) x = TerenaryBranch a (expand b x) (expand c x) (expand d x)


--trav, tutaj zrobiłem helper w postaci travHelper, żeby ładnie się
--to wyprintowało. Na pierwszym miejscu mamy liczbę liści,
--na drugim mamy liste binarnych węzłów, na trzecim listę potrójnych węzłów.
--W zależności gdzie jesteśmy tam dodajemy 1 do wyniku. Możemy używać "surowej"
--Wersji zwracającej Writera, jakby ktoś się chciał nim pobawić albo po prostu
--"Ładniejszego" trav wyciągającego Z niego potrzebne wartości. Core to i tak travHelper, 
--reszta to estetyka.
getFst (a,b,c) = getSum a
getSnd (a,b,c) = getSum b
getTrd (a,b,c) = getSum c

trav tree = [("Leafs",getFst result), ("Binary", getSnd result), ("Ternary", getTrd result)]
            where result = snd (runWriter (travHelper tree))

travHelper::(MyTree a) -> Writer (Sum Int, Sum Int, Sum Int) (MyTree a)
travHelper (Leaf a) = do tell (Sum 1,Sum 0,Sum 0); return (Leaf a)
travHelper (BinaryBranch a b c) = do tell (Sum 0,Sum 1,Sum 0); travHelper b; travHelper c; return (BinaryBranch a b c)
travHelper (TerenaryBranch a b c d) = do tell (Sum 0,Sum 0,Sum 1); travHelper b; travHelper c; travHelper d; return (TerenaryBranch a b c d)

--Podobnie jak wcześniej, helper istenieje tylko do celów estetycznych
--Bo zakładam, że jeżeli ktoś prosi o głębokość drzewa, to jednak
--Chciałby dostać liczbę, a nie całą Monadę. Ale tak jak poprzednio
--Zwracanie monady również jest opcją jakby ktoś się chciał nią bawić.
depth tree = getMax (snd (runWriter (depthHealper tree 0)))

depthHealper::(MyTree a)->Int->Writer (Max Int) (MyTree a)
depthHealper (Leaf a) n = do tell (Max n); return (Leaf a)
depthHealper (BinaryBranch a b c) n = do depthHealper b (n+1); depthHealper c (n+1); return (BinaryBranch a b c)
depthHealper (TerenaryBranch a b c d) n = do depthHealper b (n+1); depthHealper c (n+1); depthHealper d (n+1); return (TerenaryBranch a b c d) 