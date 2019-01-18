-- Implement the following functions using higher-order functions (and other predefined functions) of Haskell without using recursion.
-- Implement a function eql :: [Int] -> [Int] -> Bool that tells wether two lists of integers are equal.
eql :: [Int] -> [Int] -> Bool
eql l1 l2 = (length l1 == length l2) && (all (== True) $ zipWith (==) l1 l2)

-- Implement a function prod :: [Int] -> Int that returns the product of a list of integers.
prod :: [Int] -> Int
prod []         = 1
prod (x : xs)   = foldl (*) x xs    -- prod l = foldl (*) 1 l

-- Implement a function prodOfEvens :: [Int] -> Int that returns the product of all even numbers of a list of integers.
prodOfEvens :: [Int] -> Int
prodOfEvens l = foldl (*) 1 (filter even l)

-- Implement a function powersOf2 :: [Int] that generates the list of all the powers of 2.
powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

-- Implement a function scalarProduct :: [Float] -> [Float] -> Float that returns the dot product of two lists of float numbers with the same size.
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l1 l2 = sum $ zipWith (*) l1 l2