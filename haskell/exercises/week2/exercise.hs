{-
2. Mergesort

Write a function that sorts a list using mergesort.
-}
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

is_empty :: [a] -> Bool
is_empty list 
    | length list == 0 = True
    | otherwise = False

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort first) (mergesort second)
    where first = take half xs
          second = drop half xs
          half = length xs `div` 2

{-
3. Quicksort
Write a function that sorts a list using quicksort.
-}
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let leftList = quicksort [a | a <- xs, a <= x]
        rightList = quicksort [b | b <- xs, b > x]
    in leftList ++ [x] ++ rightList

{-
4. Vector Sum
Write a function add_vec that adds two vectors represented as lists. Give your 
function the most general possible type.
-}
add_vec :: Num a => [a] -> [a] -> [a]
add_vec list1 list2 = [x * y | (x, y) <- zip list1 list2]

{-
5. Dot Product
Write a function dot that computes the dot product of two vectors represented as lists.
-}
dot_product :: Num a => [a] -> [a] -> a
dot_product list1 list2 = sum [x * y | (x,y) <- zip list1 list2]

{-
6. Matrix Addition
Write a function add that adds two matrices represented as lists of lists.
-}
checkRows :: [[a]] -> [[a]] -> Bool
checkRows [] [] = True
checkRows (x:xs) (y:ys) = if length x == length y then checkRows xs ys else False

checkDimensions :: [[a]] -> [[a]] -> Bool
checkDimensions m1 m2 = 
    if length m1 == length m2 && checkRows m1 m2 then True else False

matrix_addition :: Num a => [[a]] -> [[a]] -> [[a]]
matrix_addition m1 m2
    | checkDimensions m1 m2 == False = error "Matrices should be of the same dimension"
    | otherwise = [[ x + y | (x,y) <- zip row1 row2 ] | (row1, row2) <- zip m1 m2]

{-
7. All Pairs
Construct an infinite list allPairs that contains all pairs of positive integers. 
Every pair must appear exactly once in the list.
-}
allPairs :: [(Integer, Integer)]
allPairs = [(x, sum - x) | sum <- [2..], x <- [1..sum - 1]]

{-
8. Floor
The RealFrac type class includes a function floor that rounds a value down to 
the nearest integer:

floor :: (RealFrac a, Integral b) => a -> b
Assume that floor is not predefined, and write it yourself.
-}
my_floor :: (RealFrac a, Integral b) => a -> b
my_floor x = head [l | l <- [1..], fromIntegral l > x] - 1  -- Runs in O(n)

get_upper_bound :: (Integral a1, Num a2, Ord a2) => a2 -> a1
get_upper_bound x = head [p | l <- [1..], let p = 2 ^ l, fromIntegral p > x]

