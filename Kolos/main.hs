-- Marek Traczyński (261748)
-- Programowanie funkcyjne
-- Kolokwium 2



--------------------
-- Import modułów --
--------------------

import Control.Monad.Writer
import Data.Semigroup



--------------------
-- Definicja typu --
--------------------

data Tree a = Leaf a 
              | DoubleBranch a (Tree a) (Tree a) 
              | TripleBranch a (Tree a) (Tree a) (Tree a) 
              deriving (Eq)



--------------------
-- Testowe drzewa --
--------------------

testTree1 = DoubleBranch 5 (Leaf 6) (Leaf 7)
testTree2 = TripleBranch 3 (Leaf 2) (Leaf 1) (testTree1)
testTree3 = DoubleBranch 10 (testTree1) (testTree2)
testTree4 = TripleBranch 3 (TripleBranch 10 (testTree3) (testTree2) (testTree1)) (testTree3) (testTree2)



--------------------
-- Instancje klas --
--------------------

{-
    Klasa Show

    Wartości liści są w kwadratowych nawiasach. Wartości węzłów z rozgałęzieniami
    są w klamrowych nawiasach, a ich rozgałęzienia są oznaczane jako 
    DoubleBranch->(Tree : Tree) lub TripleBranch=>(Tree : Tree : Tree). 


    Przykłady:

    show (Leaf 5) = [5]  
    show (DoubleBranch 5 (Leaf 4) (Leaf 3)) = {5}->(4 : 3)  
    show (TripleBranch 5 (Leaf 4) (Leaf 3) (Leaf 2)) = {5}=>(4 : 3 : 2)  
-}

instance (Show a) => Show (Tree a) where
    show (Leaf a)                   = "[" ++ show a ++ "]"
    show (DoubleBranch a b c)       = "{" ++ show a ++ "}" ++ "->(" ++ show b ++ " : " ++ show c ++ ")"
    show (TripleBranch a b c d)     = "{" ++ show a ++ "}" ++ "=>(" ++ show b ++ " : " ++ show c ++ " : " ++ show d ++ ")"


{- 
    Klasa Functor (Zadanie 2)

    Robimy fmapa na naszym drzewie, czyli jeśli mamy liść, to
    używamy funkcji f na jego wartości, a w przypadkach rozgałęzień
    również używamy funkcji f na ich wartościach, oraz rekurencyjnie
    fmap na elementach w rozgałęzieniach. 

    Funkcja testująca poprawność jest na samym dole pliku jako funkcja squareTree.
-}

instance Functor Tree where
    fmap f (Leaf a)                 = Leaf (f a) 
    fmap f (DoubleBranch a b c)     = DoubleBranch (f a) (fmap f b) (fmap f c) 
    fmap f (TripleBranch a b c d)   = TripleBranch (f a) (fmap f b) (fmap f c) (fmap f d) 



------------------
-- Funkcje typu --
------------------


{- 
    Funkcja expand (Zadanie 3) 

    Funkcja dla danej wartości value wprowadza ją do danej
    podwójnej gałęzi DoubleBranch jako liść w lewym rozgałęzieniu,
    tworząc z niej TripleBranch. Jeśli argumentem funkcji jest 
    TripleBranch, to rekurencyjnie używamy naszej funkcji na 
    wszystkich jego rozgałęzieniach, co skutkuje wprowadzeniem 
    wartości value do potencjalnych DoubleBranch'ów, które może
    on zawierać. Jeśli natrafiamy na liść to nic z nim nie robimy.
-}
expand :: a -> Tree a -> Tree a
expand _ (Leaf a)                       = Leaf a
expand value (DoubleBranch a b c)       = TripleBranch a (Leaf value) b c
expand value (TripleBranch a b c d)     = TripleBranch a (expand value b) (expand value c) (expand value d)  


{- 
    Funkcja trav z moandą Writer (Zadanie 4) 

    Funkcja przechodzi po kolejnych rozgałęzieniach w drzewie
    odpowiednio zwiększając o 1 odpowiednią sumę, w zależności
    od tego na jaki typ natrafiliśmy. Pierwsza suma zlicza nam
    liście, druga - podwójne rozgałęzienia, a trzecia - potrójne
    rozgałęzienia. 
-}
trav :: Tree a -> Writer (Sum Int, Sum Int, Sum Int) (Tree a)
trav (Leaf a)                   = do 
                                    tell (Sum 1, Sum 0, Sum 0);
                                    return (Leaf a)
trav (DoubleBranch a b c)       = do 
                                    tell (Sum 0, Sum 1, Sum 0);
                                    trav b;
                                    trav c;
                                    return (DoubleBranch a b c)
trav (TripleBranch a b c d)     = do 
                                    tell (Sum 0, Sum 0, Sum 1);
                                    trav b;
                                    trav c;
                                    trav d;
                                    return (TripleBranch a b c d)


{-
    Funkcja depth z moandą Writer (Zadanie 5)

    Funkcja porusza się po kolejnych rozgałęzieniach w drzewie
    jednocześnie zwiększając wartość akumulatora o 1 przy każdym
    przejściu poziom niżej. Gdy dotrzemy do liścia, za pomocą
    monady Writer aktualizujemy Max Inta, który zawsze będzie
    największym aktualnie znalezionym akumulatorem. Jeśli
    dochodząc do liścia nasz akumulator jest mniejszy niż
    aktualny Max Int, to pozostanie on wtedy bez zmian.
-}
depth :: Tree a -> Int -> Writer (Max Int) (Tree a)
depth (Leaf a) acc                  = do 
                                        tell (Max acc);
                                        return (Leaf a)
depth (DoubleBranch a b c) acc      = do 
                                        depth b (acc + 1);
                                        depth c (acc + 1);
                                        return (DoubleBranch a b c)
depth (TripleBranch a b c d) acc    = do 
                                        depth b (acc + 1);
                                        depth c (acc + 1);
                                        depth d (acc + 1);
                                        return (TripleBranch a b c d)


{-
    Dodatkowa funkcja zwracająca wysokość drzewa, gdzie jako argument
    podajemy samo drzewo, bez akumulatora. Zwracany wynik jest również
    Intem, a nie Writerem. 
-}
getDepth :: Tree a -> Int
getDepth tree = getMax $ execWriter $ depth tree 0 


{-
    Dodatkowa funkcja podnosząca do kwadratu wszystkie wartości w naszym
    drzewie (test poprawnego działania funktora).
-}
squareTree tree = fmap (^2) tree