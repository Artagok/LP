-- This problem explores the definition of high-order functions on lists. Implement the followinf functions that work as the original 
-- Haskell functions without using the original function eachself (i.e., you cannot use foldl ti implement myFoldl but you can use it
-- to implement myAll). Additionally. you can only use recursion to implement myFoldl, myFoldr, myIterate, myUntil and myZip.

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ x []          = x
myFoldl f x (y : [])    = f x y
myFoldl f x (y : ys)    = myFoldl f (f x y) ys

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ x []          = x
myFoldr f x (y : [])    = f y x
myFoldr f x (y : ys)    = f y $ myFoldr f x ys      -- myFoldr f x (ax:axs) = f ax $ myFoldr f x axs

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil c f x
    | c x       = x
    | otherwise = myUntil c f (f x)

myMap :: (a -> b) -> [a] -> [b]
myMap f l = [f x | x <- l]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter c l = [x | x <- l, c x]

myAll :: (a -> Bool) -> [a] -> Bool
myAll c l = and $ myMap c l                         -- amb length es massa ineficient

myAny :: (a -> Bool) -> [a] -> Bool
myAny c l = not $ null $ filter c l

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _          = []
myZip _ []          = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f l1 l2 = [f a b | (a,b) <- myZip l1 l2]