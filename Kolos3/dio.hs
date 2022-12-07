import Control.Monad.Writer
import Data.Semigroup 

--Jakby to powiedział profesor, jest to rozwiązanie sztuczne, raczej lipne, ale tylko na takie mogłem
--sobie w tym pośpiechu pozwolić, gdyż przez pewne okoliczności mój czas na to kolokwium został mocno
--skrócony. Lepiej by już pewnie było, jakbym sobie odpuścił tego Writera..., przepraszam
gcd_extended a 0 = do tell ("[" ++ "a = " ++ (show a) ++ ", b = " ++ "0" ++ "]"); return (a,1,0)
gcd_extended a b = do tell ("[" ++ "a = " ++ (show a) ++ ", b = " ++ (show b) ++ "]" ++ " =>  " ++ res); return (g,y, x - (a `div` b)*y) where
     mon = runWriter (gcd_extended b (a `mod` b)); (g,x,y) = fst mon; res = snd mon

getXY (g,x,y) = (x,y)

--Wrapper który "rozpakowuje" monadę
solveLDE a b c = ((x*g,y*g), trace) where
    mon = runWriter (gcd_extended a b); (g,x,y) = fst mon; trace = (snd mon)

--Inverse jako że jest dosyć prosty, nawet z IO, to się udało zrobić w 2 sekundy
--Powinno być w porządku, przynajmniej ja nie mam zastrzerzeń
inverseHelper x p = ((r `mod` p) + p) `mod` p where
    mon = runWriter (gcd_extended x p); (_,r,_) = fst mon

inverseP = do
  putStr "x: "
  input1 <- getLine
  putStr "p: "
  input2 <- getLine
  let x = (read input1 :: Int)
  let p = (read input2 :: Int)
  putStr "Modular inverse: "
  print $ (inverseHelper x p)
