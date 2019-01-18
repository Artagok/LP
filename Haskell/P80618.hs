-- Implement generic queues using the following interface:

data Queue a = Queue [a] [a]
    deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue l1 l2) = Queue l1 $ a : l2

pop :: Queue a -> Queue a
pop (Queue [] [])       = Queue [] [] 
pop (Queue [] l2)       = Queue (reverse $ init l2) []
pop (Queue (x : xs) l2) = Queue xs l2

top :: Queue a -> a
top (Queue (x : xs) l2) = x 
top (Queue [] l2)       = top $ Queue (reverse l2) []

empty :: Queue a -> Bool
empty (Queue l1 l2) = null l1 && null l2

-- Define equality for queues so that two queues are equal if and only if they have the same elements, in the same order, 
-- independently of their representation. In order to do so, write that queues are an instance of the Eq class, where (==) is the equality operation you have to define:
instance Eq a => Eq (Queue a)
    where 
        (Queue l1 l2) == (Queue r1 r2) = l1' == r1'
            where
                l1' = l1 ++ reverse l2
                r1' = r1 ++ reverse r2

-- Observe that, in order to have Queue a be an instance of Eq, it is necessary to have that the elements of type a are them-selves also instances of Eq.