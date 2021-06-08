import Data.Char

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

{-
5. Deduplicate
Write a function dedup that eliminates consecutive duplicate elements in a list:

> dedup [2, 4, 4, 4, 6, 6, 8, 4]
[2, 4, 6, 8, 4]
-}

dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup [x] = [x]
dedup (x:y:xs) = 
    if x == y then dedup (y:xs) else x : dedup(y:xs)

{-
7. Infinite Ints
Construct a list ints of integers from 1 to ∞. Do not use the built-in range 
operator (i.e. [1 .. ]).
-}
inf_list :: Integer -> [Integer]
inf_list start = start : inf_list (start + 1) 

{-
8. Cyclic List
Implelement the built-in function cycle that takes a list L and returns an 
infinite list consisting of L repeated over and over:

> take 10 (cycle "abc")
"abcabcabca"
-}
cycle1 :: [Char] -> [Char]
cycle1 word = word ++ cycle1 word

{-
9. Prime
Write a function that determines whether an integer is prime.
-}
tester :: Integer -> Integer -> Bool
tester i x = 
    if i * i <= x then 
        (if x `mod` i == 0 || x `mod` (i + 2) == 0 then False else tester (i + 6) x)
        else True
                

is_prime :: Integer -> Bool
is_prime number 
    | number < 2 = False
    | number == 2 = True
    | number == 3 = True
    | number `mod` 2 == 0 || number `mod` 3 == 0 = False
    | tester 5 number == False = False
    | otherwise = True 

{-
10. All Primes
Construct an infinite list containing all prime numbers.
-}

prime_cycle :: Integer -> [Integer]
prime_cycle start = if is_prime start 
    then start : prime_cycle (start + 1) 
    else prime_cycle (start + 1)

{-
11. Words
Write a function that breaks a string into a list of words.
-}
word_list :: String -> [String]
word_list = words

{-
Homework Section
-}
-- 1) LIST STRINGS 
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y : ys) 
    | x < y = x : y : ys
    | x > y = y : insert x ys
    | otherwise = y : ys

insertion_sort :: [Int] -> [Int]
insertion_sort [x] = [x]
insertion_sort (x : xs) = insert x (insertion_sort xs)

capitalHelp :: [String] -> String
capitalHelp [] = ""
capitalHelp (x:xs)
    | length (x:xs) == 1 = (toUpper (head x) : (drop 1 x))
    | isUpper (head x) = x ++ " " ++ capitalHelp xs
    | otherwise = (toUpper (head x) : (drop 1 x)) ++ " " ++ capitalHelp xs

capitalize :: String -> String
capitalize text = 
    let splitText = words text 
    in capitalHelp splitText 

prime :: Integer -> Bool
prime number 
    | number < 2 = False
    | number == 2 || number == 3 = True 
    | number `mod` 2 == 0 || number `mod` 3 == 0 = False
    | tester 5 number == False = False
    | otherwise = True 

allPrimes :: [Integer] 
allPrimes = prime_cycle 2

getPrimeUntilUpperBound :: Integer -> [Integer] 
getPrimeUntilUpperBound 1 = []
getPrimeUntilUpperBound x 
    | prime x = x : getPrimeUntilUpperBound (x-1)
    | otherwise = getPrimeUntilUpperBound (x-1)

twinHelper :: [Integer] -> Integer
twinHelper [x] = 0
twinHelper (x:y:xs) = if abs(x - y) == 2 then 1 + twinHelper (y : xs)
                                            else twinHelper (y : xs)

twinPrimes :: Integer -> Integer
twinPrimes upperBound = 
    let list = getPrimeUntilUpperBound upperBound
    in if length list == 0 || length list == 1 then 0
        else twinHelper list
