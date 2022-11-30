-- 1. Implementacja ciała skończonego o zadanym orderze i operacji na nim.
data Field2137 = Elem Integer 

instance Eq Field2137 where
    (Elem a) == (Elem b) = (a `mod` 2137) == (b `mod` 2137)

instance Show Field2137 where
    show (Elem a) = show (a `mod` 2137)

instance Num Field2137 where
    (Elem a) + (Elem b) = Elem ((a + b) `mod` 2137)
    (Elem a) - (Elem b) = Elem ((a - b ) `mod` 2137)
    (Elem a) * (Elem b) = Elem ((a * b) `mod` 2137)
    signum (Elem a) = 1
    fromInteger a = Elem (fromInteger ((a `mod` 2137)))
    abs (Elem a) = (Elem (a `mod` 2137))

moduloInverse a = (z^(2135)) `mod` 2137 where z = a `mod` 2137


instance Fractional Field2137 where
    (Elem a) / (Elem b) = Elem ( (a * (moduloInverse b)) `mod` 2137)
