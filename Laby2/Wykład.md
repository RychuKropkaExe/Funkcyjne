Listy:
```hs
(++): [x1,...,xn] ++ [y1,...,yn] = [x1,...,xn,y1,...,yn]
      []++ys = ys
      (x:xs)++ys = x:(xs++ys)
    [x1,x2,x3]++ys = x1:([x2,x3]++ys) = x1:x2:([x3]++ys) = x1:x2:x3:ys
```
2)
```hs
map f xs = [fx| x <- xs]
```
3)
```hs
filter :: (a -> Bool) -> [a] -> [a]
  filter _ [] = []
  fiter p (x:xs)
    |p x = x:filter p xs
    |otherwise = filter p xs
```
4)
```hs
elem :: a->[a]->Bool
elem x [] = False
elem x (y:ys) = if(x == y) then True
                else elem x y
>:t elem
elem (Eq a) => (a->[a]->Bool)
```
Klasa typów Eq to typy z zaimplementowaną równością
5)
```hs
zip [1,2,3] ['a', 'b'] = [(1,'a'),(2,'b')]
zip _ [] = []
zip [] _ = []
zip (x:xs) (y:ys) = (x,y):zip xs ys 
```

6)
```hs
zipWith::(a->b->c)->[a]->[b]->[c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = (f x y):zipWiths f xs ys
równoznacznie:
zipWith f x y = map f (zip x y)
```
Przykład
```hs
zip [1..] ['a', 'c', 'b'] = [(1,'a'), (2,'c'),(3,'b')]
```

W GHCI:
```hs
>xx = [1,3..] :: [Integer]
>:sprint xx
xx = _
>take 5 xx
1 3 5 7 9
>:sprint xx
xx = 1:3:5:7:9:_

Przyklad: Liczby Fibonacciego: F0 = 0, F1 = 1, Fn = Fn-2 + Fn-1

```hs
  fib = [F0,F1,F2,F3,..] = 0:1:[F2,F3,F4,..] = 0:1:[F0+F1,F1+F2,F2+F3,...] = 0:1:fib + (tail fib)
  fib = 0:1:zipWith (+) fib (tail fib)
```

SPŁASZCZANIE
```
[x1,x2,x3,x4] : x1(+)(x2(+)(x3(+)x4))
e,[x1,x2,x3,x4] = x1(+)(x2(+)(x3(+)(x4(+)e))) <- Fold prawy
```
```hs
foldr :: (a->b->b)->b->[a]->b
foldr _ b [] = b
foldr f b (x:xs) = f x (foldr f b xs)
```

```
foldr (+) 0 xs = sum xs
foldr (*) 1 xs = product xs
fact n = product [1..n]
sum = foldr (+) 0
length xs = fold (\x n -> n +1) 0 xs
and = foldr (&&) True
or = foldr (||) False
```

Generalnie foldy fajne

7)
```hs
reverse xs = foldr (\x xs -> xs++[x]) [] xs
```

```
[x1,x2,x3,x4] -> (((e(+)x1)(+)x2)(+)x3)(+)x4 = ((x'(+)x2)(+)x3)(+)x4
```

```hs
foldl _ e [] = e
foldl f e (x:xs) = foldl f (f e x) xs 
```

```hs
reverse xs = foldl (\xs x -> x:xs) [] xs
f = (\xs x -> x:xs)
flip f x y = f y x
flip (:) xs x = (:)x xs = x:xs
reverse xs = foldl (flip (:)) [] xs
```

```
(+) - łączne
e - el. neutralny obustronnie
foldl (+) e = foldr (+) e
```

foldl vs foldr

```hs
foldl (/) 1 [x1,x2,x3,x4] = 1/(x1*x2*x3*x4)
foldr (/) 1 [x1,x2,x3,x4] = x1/(x2/(x3/(x4/1))) = (x1*x3)/(x2*x4)
```

Elementy teorii kategorii
```
Kategoria: 1) klasa obiektów (ob(e))
           2) klasa morfizmów (mor(e))
              (strzałki)
              f - morfizm dom(f) {isin} ob(e)
                          codom(f) {isin} ob(e)
              [f: X -> Y = X ->f Y = dom(f) = X
                                     codom(f) = Y]
           3) mamy określone składanie o
              f: X -> Y|
              g: Y -> Z|  => g o f: X -> Z
           4) dla każdego X{isin}ob(e) mamy dokładnie
              jeden morfizm idx: X -> X, czyli morfizm:
                  (VY)(Vf:X->Y)(f o idx = f)
                  $$(VZ)(Vg:Z->X)(id_x o g = g)$$
```      
Przykład:
Set : obiekty = zbiory

$$f: X->Y = dom(f) = X i rng(f) \subset Y$$

      Subtelność:
        f: N->N : n->n dom(f) = N, codon(f) = N
        g: N -> R : n->n dom(g) = N; codom(g) = R
          function f(int n):int {return n}
          function g(int n):float {return n}
Przykład:
$$(X, <=) - cz. porz$$
$$ PO(X) : obiekty === elementy X$$
$$ morfizmy = {a,b} \epsilon X^2 : a <= b$$
Przykład:

Grp : obiekty === grupy 
      morfizmy === homomomorfizmy
Def. Obiekty X, Y są izomorficzne,
$$ Jeżeli istnieją f: X -> Y i g: Y -> X t. że $$
$$ g o f = id_{x}$$
$$ f o g = id_{y}$$

Tw. Zał 
$$f: X -> Y, g_{1}: Y->X, g_{2}:Y->X$$
$$fog=id_{Y}$$
$$g_{1}of-id_{x}$$
$$fog_{2} = id_{y}$$
$$g_{2}of = id_{x}$$
Wtedy:
$$g_{1} = g_{2}$$
Def. K jest obiektem końcowym w e jezeli:
$$ (\bigvee X)(\bigwedge !f)(f:X->K)$$

Fakt. Jeżeli 

$$ K_{1},K_{2}$$
są końcowe to:

$$ K_{1} \simeq K_{2} $$

Haskell: ( ) - zero-bitowy typ
Bool = {False, True}


