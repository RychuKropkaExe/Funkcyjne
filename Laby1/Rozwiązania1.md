16.
```haskell
euler n = length([k | k <- [1..n], gcd k n == 1])

euler_sum = sum([euler k | k <- [1..n], mod n k == 0])
```
17.
```haskell
    pit_thirds = [(a,b,c) | a <- [1..200], b <- [1..a], c <- [1..a], (a^2) == (b^2) + (c^2), gcd b c == 1]
```
18.
```haskell
    help 0 (x,y) = x
    help n (x,y) = help (n-1) (y, (x+y))

    fib n = help n 0 1
    fib n = fst(fib n)
    fibb = (0,1):map (\(x,y) -> (y,x+y)) fibb::[(Integer, Integer)]

```
20.
```haskell
perfect = [ n | n <- [1..10000], sum([k | k <- [1..n-1], mod n k == 0]) == n]
```

25.
```haskell
import Data.List
    sort "Ala ma"
    nub [1,1,2,1,3,1,1,5,6]
    my_reverse [] = []
    my_reverse (x:xs) = my_reverse(xs) ++[x]

```

