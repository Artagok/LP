-- Implement the following functions using higher-order functions (and other predefined functions) of Haskell without using recursion.
-- Implement a function flatten :: [[Int]] -> [Int] that flattens a list of lists of integers in a list of integers.
flatten :: [[Int]] -> [Int]
flatten l = foldl (++) [] l     -- flatten = concat (higher-order function per concatenar llistes de llistes)

-- Implement a function myLength :: String -> Int that returns the length of a string.
myLength :: String -> Int
myLength l = foldl (\x _ -> x + 1) 0 l -- Cal posar _ a la lambda anonima per anar ciclant els Chars del String, simplement els ciclem i no els apliquem res, anem sumant 1

-- Implement a function myReverse :: [Int] -> [Int] that reverses a list of integers.
myReverse :: [Int] -> [Int]
myReverse l = foldl (flip (:) ) [] l -- No funciona amb el foldr, SI amb foldl !! ___ myReverse l = foldl (\xs x -> x:xs) [] l

-- Implement a function countIn :: [[Int]] -> Int -> [Int] that, given a list of sublists ℓ and an element x, returns the list that 
-- tells who many times x appears in each sublist of ℓ.
countIn :: [[Int]] -> Int -> [Int]
countIn l x = map length $ map (filter (== x)) l

-- Implement a function firstWord :: String -> String that, given a string with blanks and alphabetic characters, returns its first word.
firstWord :: String -> String
firstWord = takeWhile (/= ' ') . dropWhile (== ' ')