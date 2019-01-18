-- In this problem you should implemet a series of functions using comprehension lists.
-- Implement a function myMap :: (a -> b) -> [a] -> [b] that emulates map using comprehension lists.
myMap :: (a -> b) -> [a] -> [b]
myMap f l = [f x | x <- l]

-- Implement a function myFilter :: (a -> Bool) -> [a] -> [a] that emulates filter using comprehension lists.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = [x | x <- l, f x]

-- Implement a function myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] that emulates zipWith using comprehension lists and zip.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f l1 l2 = [f (fst x) (snd x) | x <- zip l1 l2]        -- tambe [f x y | (x,y) <- zip l1 l2]

-- Implement a function thingify :: [Int] -> [Int] -> [(Int, Int)] that, given two lists of integers, returns the list 
-- that pairs the elements if the element of the second list divides the one in the first list.
thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify l1 l2 = [(a,b) | a <- l1, b <- l2, a `mod` b == 0]

-- Implement a function factors :: Int -> [Int] that, given a non-null natural number, generates the ordered list with all its factors (non necessaryly primes).
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]