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


