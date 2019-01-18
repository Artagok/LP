-- The goal of this problem is to work the definition of infinite lists. In particular, you are required to define functions that generate infinite lists to:

-- Generate the sequence of ones [1,1,1,1,1,1,1,1,…].
ones :: [Integer]
ones = iterate (const 1) 1 -- iterate (+0) 1

-- Generate the sequence of the natural numbers [0,1,2,3,4,5,6,7…].
nats :: [Integer]
nats = iterate (+1) 0

-- Generate the sequence of the integer numbers [0,1,−1,2,−2,3,−3,4…].
ints :: [Integer]
ints = 0 : concatMap (\x -> [x, -x]) (iterate (+1) 1)

-- Generate the sequence of the triangular numbers: 0,1,3,6,10,15,21,28,…].
triangulars :: [Integer]
triangulars = scanl (+) 0 $ iterate (+1) 1

-- Generate the sequence of the factorial numbers: [1,1,2,6,24,120,720,5040,…].
factorials :: [Integer]
factorials = scanl (*) 1 $ iterate (+1) 1

-- Generate the sequence of the Fibonacci numbers: [0,1,1,2,3,5,8,13,…].
fibs :: [Integer]
fibs = [0,1] ++ generateFibs 0 1 -- fibs = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)

generateFibs :: Integer -> Integer -> [Integer]
generateFibs f s = (f+s) : generateFibs s (f+s)

-- Generate the sequence of prime numbers: [2,3,5,7,11,13,17,19,…].
primes :: [Integer]
primes = filter (\x -> isPrime x 2) $ iterate (+1) 2

isPrime :: Integer -> Integer -> Bool
isPrime x n
    | (n ^ 2) > x       = True
    | x `mod` n == 0    = False
    | otherwise         = isPrime x (n+1)