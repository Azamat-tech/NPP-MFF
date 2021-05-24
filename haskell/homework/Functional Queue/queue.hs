class Queue q where 
    emptyQueue :: q a
    isEmpty ::  q a -> Bool
    enqueue :: a -> q a -> q a
    dequeue :: q a -> (a, q a)

data MyQueue a = Null | Cons a (MyQueue a)

instance Queue MyQueue where 
    emptyQueue = Null
    
    isEmpty Null = True

    enqueue x Null = Cons x (Null)
    enqueue x  (Cons y ys) = Cons y (enqueue x ys)

    dequeue Null = error "YO it's empty"
    dequeue (Cons x y) = (x,y)

data SQueue a = Q [a] [a]

-- type Empty = Q [] []

instance Queue SQueue where 
    emptyQueue = Q [] []

    isEmpty (Q [] []) = True
    isEmpty (Q _ _) = False

    enqueue x (Q xs []) = Q xs [x]
    enqueue x (Q xs y) = Q xs (x:y)

    dequeue (Q [] []) = error "YO it's empty"
    dequeue (Q [] xs) = dequeue(Q (reverse xs) [])
    dequeue (Q (x:xs) ys) = (x, Q xs ys)

instance (Eq a) => Eq (SQueue a) where
    (Q a b) == (Q c d) = a ++ b == c ++ d

instance (Show a) => Show (SQueue a) where 
    show (Q a b) = "q" ++ show (a ++ reverse b)

instance Functor SQueue where 
    fmap _ (Q [] []) = Q [] []
    fmap f (Q xs ys) = (Q (fmap f xs) (fmap f ys))

-- queueH :: Queue q => q Int -> Int -> Int -> q Int
-- queueH q from to = 
--     if from /= to then 
--         let newQ = enqueue from q
--         in queueH newQ (from + 1) to
--     else enqueue to q

-- queue_of_nums :: Queue q => Int -> Int -> q Int
-- queue_of_nums from to = queueH emptyQueue from to

queue_of_nums :: Queue q => Int -> Int -> q Int
queue_of_nums from to = foldr enqueue emptyQueue (reverse [from..to])
    
test = 
    let q = emptyQueue :: SQueue Int
        q1 = enqueue 5 q
        q2 = enqueue 10 q1
        e = isEmpty q2
        (x, q3) = dequeue q2
        (y, q4) = dequeue q3
        f = isEmpty q4
    in (e, x, y, f)
