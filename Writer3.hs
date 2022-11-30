{-- Teraz korzystamy z zaimplementowanej w module Control.Monad.Writer
    wersji Writera
--}    

import Control.Monad.Writer
import Data.Semigroup  -- (będziemy korzystali z monoidu Max Int)

-----------------------------------------------------  

myGCD :: Int -> Int -> Writer [String] Int  
myGCD a b  
    | b == 0 = do  
        tell ["GCD = " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        myGCD b (a `mod` b)  

-- Implementujemy funkcję Collatz'a ze śledzeniem
-- obliczeń; śledzimy 
--  1. liczby, które się pojawiają w trakcie obliczeń
--  2. liczbę kroków  
-- Korzystamy z produktu dwóch monoidów: 
--         String x (Sum Int)

collatz :: Int -> Writer (String, Sum Int) Int
collatz n | n==1  = do tell (show n, 0); return 1
          | n `mod` 2 == 0  = do 
               tell (show n ++ " ", 1); 
               collatz (n `div` 2)
          | otherwise       = do 
               tell (show n ++ " ", 1); 
               collatz (3*n+1)

-- Implementujemy funkcję Collatz'a ze śledzeniem
-- obliczeń; śledzimy 
--  1. liczby, które się pojawiają w trakcie obliczeń
--  2. wykonywane operacje 
--  3. maksymalną liczbą pojawiającą się w trakcie obliczeń
--  4. liczbę kroków  
-- Korzystamy z monoidu który jest produktem czterech monoidów
--     String x String x (Max Int) x (Sum Int)
       
collatz' :: Int -> Writer (String, String, Max Int,  Sum Int) Int
collatz' n | n==1  = do tell (show n,"",Max n, 0); return 1
          | n `mod` 2 == 0  = do 
               tell (show n++";","D",Max n, 1); 
               collatz' (n `div` 2)
          | otherwise       = do 
               tell (show n++";","M", Max n, 1); 
               collatz' (3*n+1)



