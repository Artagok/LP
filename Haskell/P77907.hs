absValue :: Int -> Int
absValue x = abs x

power :: Int -> Int -> Int
power x n = x ^ n

isPrime :: Int -> Bool
isPrime x 
    | (x == 0 || x == 1)   = False 
    | otherwise            = isPrime' x 2 

isPrime' :: Int -> Int -> Bool
isPrime' x n
    | (n ^ 2) > x       = True
    | x `mod` n == 0    = False
    | otherwise         = isPrime' x (n+1)

slowFib :: Int -> Int
slowFib n
    | n == 0        = 0
    | n == 1        = 1
    | otherwise     = slowFib (n-1) + slowFib (n-2)

quickFib :: Int -> Int
quickFib n = fst (quickFib' n)
    where
        quickFib' 0     = (0, 0)
        quickFib' 1     = (1, 0)
        quickFib' n     = (res + ant, res)
            where
                (res, ant) = quickFib' (n-1)