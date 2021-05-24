import Data.List

reflexive :: Eq a => (a -> a -> Bool) -> [a] -> Bool
reflexive _ [] = True
reflexive f (x:xs)
    | f x x = reflexive f xs
    | otherwise = False
-- reflexive f xs = and [elem x xs | x <- xs, y <- xs, x == y, f x y]

symmetric :: Eq a => (a -> a -> Bool) -> [(a,a)] -> Bool
symmetric _ [] = True
symmetric f ((x,y):xs)
    | f x y == f y x = symmetric f xs
    | otherwise = False
-- symmetric f xs = and [elem (x,y) xs | (y,x) <- xs]

transitive :: Ord a => [(a,a)] -> Bool
transitive xs = and [elem (x, w) xs | (x,  y) <- xs, (z, w) <- xs, y == z]

is_equiv :: Ord a => (a -> a -> Bool) -> [a] -> Bool
is_equiv f xs = 
    let cartesion_product = [(x,y) | x <- xs, y <- xs, f x y]
    in if reflexive f xs && symmetric f cartesion_product && transitive cartesion_product 
        then True 
        else False

partition_list :: (a -> a -> Bool) -> [a] -> ([a], [a])
partition_list _ [] = ([],[])
partition_list f (x:xs) = partition (f x) (x:xs)

classes :: (a -> a -> Bool) -> [a] -> [[a]]
classes _ [] = []
classes f xs = 
    let (passed_parition, failed_partition) = partition_list f xs
    in passed_parition : classes f failed_partition

reflexive_closure :: Eq a => (a -> a -> Bool) -> (a -> a -> Bool)
reflexive_closure f = 
    let g x y = f x y || x ==  
    in g