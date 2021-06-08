import Data.List
{-
Implementation of max function of Ord
-}
maximum1 :: (Ord a) => [a] -> a
maximum1 [] = error "The list is empty"
maximum1 [x] = x
maximum1 (x:xs) 
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum1 xs

{-
find the sum of all odd squares that are smaller than 10 000
-}
result :: Integer
result = sum $ takeWhile (<10000) $ filter odd $ map (^2) [1..]

reverse1 :: [a] -> [a]
reverse1 = foldl (\ acc x -> x : acc) []

{-
1. Curried Functions - CLASS WORK
A curried function takes its arguments one at a time:

> sum 3 4
7
An uncurried function takes its arguments in a tuple:

> sum (3, 4)
7
Write a function curry that converts an uncurried function of two arguments to
a curried function, and a function uncurry that performs the inverse transformation.
-}
curry1 :: ((a,b) -> c) -> a -> b -> c
curry1 f x y = f (x, y)

uncurry1 :: (a -> b -> c) -> ((a, b) -> c)
uncurry1 f (x, y) = f x y

{-
2. Iterate - CLASS WORK
Write a function iterate f x that returns an infinite list of repeated applications of f to x:

> take 10 (iterate (*2) 1)
[1,2,4,8,16,32,64,128,256,512]
-}
iterate1 :: (a -> a) -> a -> [a]
iterate1 f x = x : iterate1 f (f x)

{-
3. sortOn
Write a function sortOn f list that sorts a list by applying a key function to 
each element:

> sortOn length ["one", "two", "three", "four", "five", "six"]
["one","two","six","four","five","three"]
As in this example, the sort should be stable: elements with the same key value 
should keep their relative order.
-}
sortOn1 :: Ord a => Ord b => (a -> b) -> [a] -> [a]
sortOn1 f list = 
    let nList = zip3 (map f list) [1..] list
    in [x | (_,_,x) <- sort nList]

{-
4. Alternating Map
Write a function altMap that takes two functions and a list. altMap should 
apply the two functions alternately to list elements. For example:

altMap (\i -> i + 1) (\i -> i - 1) [1..8] == [2, 1, 4, 3, 6, 5, 8, 7]
-}
-- Recusrive solution
helper :: Integral a => (a -> a) -> (a -> a) -> [(a,a)] -> [a]
helper _ _ [] = []
helper f g ((x,y):xs) 
    | even x = f y : helper f g xs
    | otherwise = g y : helper f g xs 

altMap :: Num a => Integral a => Enum a => (a -> a) -> (a -> a) -> [a] -> [a]
altMap f g l = 
    let lPairs = [(x,y) | (x,y) <- zip [0..] l]
    in helper f g lPairs


-- Pattern Matching Solution
altMap1 :: (a -> a) -> (a -> a) -> [a] -> [a]
altMap1 _ _ [] = []
altMap1 f _ [x] = [f x]
altMap1 f g (x : y : xs) = f x : g y : altMap1 f g xs 

{-
Homeworks - (Professor's Solution) 
It uses the concepts covered in the class. Thus, it was used to revise those
concepts
-}
-- Ranked-choice voting 
get_candidates :: [[String]] -> [String]
get_candidates ballots = nub(concat ballots)

elect :: [[String]] -> [String]
elect ballots = 
    let candidates = get_candidates ballots
    in if length candidates == 1 then candidates 
       else let votes_for  :: String -> Int 
                votes_for candidate = length [() | (d : _) <- ballots, d == candidate]
                (_, loser) = minimum [(votes_for c, c) | c <- candidates]
            in loser : elect [delete loser b | b <- ballots]

test = elect [
  [ "red", "green" ],
  [ "blue" ],
  [ "green", "yellow", "red", "blue" ],
  [ "blue", "green", "yellow", "red" ],
  [ "green" ]
  ]

test2 = elect [
  [ "karel", "lucie" ],
  [ "karel", "petra", "ondrej" ],
  [ "petra" ],
  [ "petra" ],
  [ "karel", "david", "petra" ],
  [ "petra" ],
  [ "lucie", "karel", "ondrej" ],
  [ "david", "karel" ],
  [ "david", "petra", "ondrej" ],
  [ "david", "lucie" ],
  [ "karel" ],
  [ "lucie" ],
  [ "petra", "ondrej", "lucie" ]
  ]

test3 = elect [
  [ "purple" ],
  [ "orange", "red", "purple" ],
  [ "blue", "orange" ],
  [ "purple", "orange" ],
  [ "red", "purple", "orange" ],
  [ "red", "blue" ],
  [ "red" ],
  [ "orange" ],
  [ "purple", "red", "blue" ],
  [ "red", "orange", "purple", "blue" ],
  [ "orange", "red", "purple" ],
  [ "red", "blue" ],
  [ "red", "purple" ],
  [ "blue" ],
  [ "purple" ],
  [ "orange", "red" ],
  [ "orange", "purple", "blue", "red" ],
  [ "red" ],
  [ "blue", "orange" ],
  [ "red" ],
  [ "purple", "blue" ],
  [ "purple", "blue" ],
  [ "blue", "red", "orange" ],
  [ "red", "purple", "blue" ]
  ]

-- Relations 
is_reflexive :: (a -> a -> Bool) -> [a] -> Bool
is_reflexive f list = and [ f x x | x <- list ]

is_symmetric :: (a -> a -> Bool) -> [a] -> Bool
is_symmetric f list = and [f x y == f y x | x <- list, y <- list ]

is_transitive :: (a -> a -> Bool) -> [a] -> Bool
is_transitive f list = and [f x z | x <- list, y <- list, z <- list, f x y, f y z]

is_equiv :: (a -> a -> Bool) -> [a] -> Bool
is_equiv f list = is_reflexive f list && is_symmetric f list && is_transitive f list 

classes :: (a -> a -> Bool) -> [a] -> [[a]]
classes _ [] = []
classes f (x : xs) = 
    let (left ,right) = partition (f x) (x : xs)
    in left : (classes f right)

reflexive_closure :: Eq a => (a -> a -> Bool) -> (a -> a -> Bool)
reflexive_closure f x y = (x == y) || f x y