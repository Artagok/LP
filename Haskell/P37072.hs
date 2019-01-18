{- In this problem you have to write several functions for generic binary trees. The definition of the trees is given by: 
   data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

That is, a tree with elements of type a is, either an empty tree, either a node with an element (of type a) and two other 
trees of the same type. The deriving (Show) statement simply enables an visualization of trees. -}

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

-- Write a function size :: Tree a -> Int that, given a tree, returns its size, that is, the number of node it contains.
size :: Tree a -> Int
size Empty          = 0
size (Node a b c)   = 1 + size b + size c

-- Write a function height :: Tree a -> Int that, given a tree, returns its height, assuming that empty trees have zero height.
height :: Tree a -> Int
height Empty          = 0
height (Node a b c)   = 1 + max b' c'
    where b' = height b
          c' = height c

-- Write a function equal :: Eq a => Tree a -> Tree a -> Bool that, given two trees, tells whether they are the same.
equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty                   = True
equal Empty _                       = False
equal _ Empty                       = False
equal (Node a al ar) (Node b bl br) = a == b && equal al bl && equal ar br

-- Write a function isomorphic :: Eq a => Tree a -> Tree a -> Bool that, given two trees, tells whether they are 
-- isomorphic, that is, if one can obtain one from the other flipping some of its descendants.
isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty                  = True
isomorphic Empty _                      = False
isomorphic _ Empty                      = False
isomorphic (Node a al ar) (Node b bl br)= a == b && (isomorphic al bl || isomorphic al br) && (isomorphic ar br || isomorphic ar bl)

-- Write a function preOrder :: Tree a -> [a] that, given a tree, return its pre-order traversal.
preOrder :: Tree a -> [a]
preOrder Empty          = []
preOrder (Node a al ar) =  a : preOrder al ++ preOrder ar

-- Write a function postOrder :: Tree a -> [a] that, given a tree, return its post-order traversal.
postOrder :: Tree a -> [a]
postOrder Empty             = []
postOrder (Node a al ar)    = postOrder al ++ postOrder ar ++ [a]

-- Write a function inOrder :: Tree a -> [a] that, given a tree, return its in-order traversal.
inOrder :: Tree a -> [a]
inOrder Empty           = []
inOrder (Node a al ar)  = inOrder al ++ [a] ++ inOrder ar

-- Write a function breadthFirst :: Tree a -> [a] that, given a tree, return its traversal by levels.
breadthFirst :: Tree a -> [a]
breadthFirst t = bfs [t]

bfs []                      = []
bfs (Empty : xs)            = bfs xs
bfs ((Node a al ar) : xs)   = a : (bfs (xs ++ [al,ar]))

-- Write a function build :: Eq a => [a] -> [a] -> Tree a that, given a pre-order traversal of a tree and an in-order traversal of
-- the same tree, returns the original tree. You can assume that the three has no repeated elements.
build :: Eq a => [a] -> [a] -> Tree a
build [] []     = Empty
build p@(px : pxs) i = Node px (build lp li) (build rp ri)
    where (li,_:ri) = span (/= px) i
          (lp,rp) = splitAt (length li) pxs

-- Write a function overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a that, given two trees, returns its overlapping using a function. 
-- Overlapping two trees with a function consists in placing the two trees one on the other and combine the double nodes using the given function.
overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty                   = Empty
overlap _ a Empty                       = a
overlap _ Empty b                       = b
overlap f (Node a al ar) (Node b bl br) = Node (f a b) (overlap f al bl) (overlap f ar br)