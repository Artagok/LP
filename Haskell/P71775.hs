-- This problem explores the definition of higher-order functions on lists.
-- Define a function countIf :: (Int -> Bool) -> [Int] -> Int that, given a predicate on integers and a list of integers, returns the 
-- number of elements in the list that satify the predicate.
countIf :: (Int -> Bool) -> [Int] -> Int
countIf c l = length $ filter c l

-- Define a function pam :: [Int] -> [Int -> Int] -> [[Int]] that, given a list of integers and a list of functions from integers to 
-- integers, returns the list consisting if applying each of the functions in the second list to the elements in the first list.
pam :: [Int] -> [Int -> Int] -> [[Int]]
pam l f = [map f' l | f' <- f]

-- Define a function pam2 :: [Int] -> [Int -> Int] -> [[Int]] that, given a list of integers and a list of functions from integers to
-- integers, returns the list of lists where each list if the result of applying, one after the other, the function in the second list to each element in the first list.
-- pam2 [1,2,3] [(+1),(*2),(^2)] ==> [[2,2,1],[3,4,4],[4,6,9]]
pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 l f = map (\x -> map ($x) f) l         -- $ means that function application can be treated just like any other function !! ==> map ($3) [(4+),(10*),(^2)]

-- Define a function filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int that returns a fold of all the 
-- elements that satisfy the given predicate.
-- filterFoldl even (*) 1 [4,7,2,4,9,3] ==> 32
filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl c f x l = foldl f x $ filter c l

-- Define a function insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int] that, given a relation between integers, a list and un element, 
-- return the list with the inserted element according to the relation.
-- Use function insert, in order to define function insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int] that orders a list according to the given relation.
insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert c l x = lfirst ++ [x] ++ lsecond      -- insert c l x = takeWhile (not.c x) l ++ [x] ++ dropWhile (not.c x) l
    where 
        lfirst  = takeWhile (\y -> c y x) l  -- lambda perque l'ordre dels parametres de c importa ==> (<) x y =/= (<) y x 
        lsecond = drop k l 
        k       = length lfirst

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int] -- insertionSort (>) [4,5,2,3,1,3]
insertionSort c l = insertionSort' c l []               -- insertionSort c l = foldr (\x xs -> insert c xs x) [] l

insertionSort' c [] la      = la
insertionSort' c (x:xs) la  = insertionSort' c xs $ insert c la x