-- Define a function insert :: [Int] -> Int -> [Int] that, given a sorted list and an element, correctly inserts the new element in the list.
-- Define a function isort :: [Int] -> [Int] that implements insertion sort using the previous function.
insert :: [Int] -> Int -> [Int]
insert [] x     = [x]
insert (y : ys) x   
    | x <= y    = x : y : ys
    | otherwise = y : insert ys x

isort :: [Int] -> [Int]
isort []        = []
isort (x : xs)  = insert (isort xs) x

-- Define a function remove :: [Int] -> Int -> [Int] that, given a list and an element x, erases the first occurrence of x from the list. 
-- You can assume that the element is always in the list.
-- Define a function ssort :: [Int] -> [Int] that implements selection sort using the previous function.
remove :: [Int] -> Int -> [Int]
remove (l : ls) x
    | x == l    = ls
    | otherwise = l : remove ls x

ssort :: [Int] -> [Int]
ssort []        = []
ssort l  = m : ssort l'
    where
        m  = minimum l
        l' = remove l m

-- Define a function merge :: [Int] -> [Int] -> [Int] that, given two sorted lists, merges them to get a list with all the elements in sorted order.
-- Define a function msort :: [Int] -> [Int] that implements merge sort using the previous function.
merge :: [Int] -> [Int] -> [Int]
merge [] l2     = l2
merge l1 []     = l1
merge l1@(x : xs) l2@(y : ys)
    | x <= y    = x : merge xs l2
    | otherwise = y : merge l1 ys

msort :: [Int] -> [Int]
msort []    = []
-- cas base en que nomes hi ha un element !! msort [1] = [1]
msort [x]   = [x]
msort l     = merge l1 l2 
        where 
            l1 = msort $ take n l
            l2 = msort $ drop n l 
            n = length l `div` 2

-- Define a function qsort :: [Int] -> [Int] that implements quick sort.
qsort :: [Int] -> [Int]
qsort []        = []
qsort [x]       = [x]
qsort (x : xs)  = qsort l1 ++ [x] ++ qsort l2
            where 
                l1 = filter (<= x) xs
                l2 = filter (> x) xs

-- Generalize the previous function into genQsort :: Ord a => [a] -> [a] that sorts elements of any type.
genQsort :: Ord a => [a] -> [a]
genQsort []     = []
genQsort [x]    = [x]
genQsort (x : xs) = genQsort l1 ++ [x] ++ genQsort l2
            where
                l1 = filter (<= x) xs
                l2 = filter (> x) xs