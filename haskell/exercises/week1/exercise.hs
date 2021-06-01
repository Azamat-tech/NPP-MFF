{-
1. Last Element
Write a function last that returns the last element of a list.
-}
last1 :: [a] -> a
last1 [x] = x
last1 (_:xs) = last1 xs

{-
2. Second Last
Write a function secondLast that returns the second-to-last element of a list.
-}
secondLast :: [a] -> a
secondLast [x,y] = x
secondLast (x:y:xs) = secondLast(y:xs)

{-
3. k-th element
Write a function kth that returns the kth element of a list, where elements are
numbered starting from 0. (Do not use the built-in operator !!, which does the same thing.)
-}
kth :: [a] -> Int -> a
kth [] _ = error "The list is empty"
kth (x:xs) 0 = x
kth (x:xs) n = kth xs (n-1)

{-
4. Zip
Implement the built-in function zip that converts two lists into a list of pairs:

> zip [10, 12, 14] [21, 23, 25, 27]
[(10,21),(12,23),(14,25)]
Stop zipping when you reach the end of either list.
-}
zip1 :: [a] -> [a] -> [(a,a)]
zip1 [] [] = []
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys 


