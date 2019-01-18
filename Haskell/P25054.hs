-- Define a function myLength :: [Int] -> Int that, given a list of integers, returns its length. 
myLength :: [Int] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- Define a function myMaximum :: [Int] -> Int that, given a non-empty list of integers, returns its maximal element.
-- Es pot fer amb la funcio sobre llistes maximum l
myMaximum :: [Int] -> Int
myMaximum (x:[])        = x
myMaximum (x:xs)
    | x >= myMaximum xs = x
    | otherwise         = myMaximum xs

-- Define a function average :: [Int] -> Float that, given a non-empty list of integers, returns its average.
average :: [Int] -> Float
average l = fromIntegral (sum l) / fromIntegral (myLength l)

-- Define a function buildPalindrome :: [Int] -> [Int] that, given a list, returns its palindrome that starts with the reserved list.
buildPalindrome :: [Int] -> [Int]
buildPalindrome l = reverse l ++ l

{- Define a function remove :: [Int] -> [Int] -> [Int] that given a list of integers x and a list of integers y, returns x after 
having removed all the ocurrences of the elements in y. -}
remove :: [Int] -> [Int] -> [Int]
--remove x y = [xs | xs <- x, not $ elem xs y] ==> usant Comprehension Lists
remove [] _ = []
remove (x:xs) y 
    | x `elem` y      = remove xs y
    | otherwise     = x : remove xs y

-- Define a function flatten :: [[Int]] -> [Int] that flattens a list of lists yielding a single list of elements.
flatten :: [[Int]] -> [Int]
-- flatten = concat ==> concat es del Prelude
flatten [] = []
flatten (x : xs) = x ++ flatten xs  -- tambe flatten (x : xs) = foldl (++) x xs

-- Define a function oddsNevens :: [Int] -> ([Int],[Int]) that, given a list of integers, returns two lists: Onw with all the even numbers 
-- and one with all the odd numbers, each of them in the same relative order as in the original list.
oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens []   = ([],[])
oddsNevens (x : xs)
    | even x    = (a, x : b)
    | otherwise = (x : a, b)
        where
            (a, b) = oddsNevens xs

-- Define a function primeDivisors :: Int -> [Int] that returns the list of prime divisors of a non-zero natural.
primeDivisors :: Int -> [Int]
primeDivisors 1     = []
primeDivisors n     = filter (\x -> (n `mod` x) == 0 && (isPrime' x 2)) [2 .. (n)]

isPrime' :: Int -> Int -> Bool
isPrime' x n
    | (n ^ 2) > x       = True
    | x `mod` n == 0    = False
    | otherwise         = isPrime' x (n+1)
