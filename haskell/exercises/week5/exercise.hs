import Prelude hiding (iterate)
import Data.List hiding (iterate)

{-
1. Infinite Iteration
The built-in function iterate produces an infinite list by repeated applying a
function to a value:

iterate (*2) 1 => [1, 2, 4, 8, ...]

Write this function.
-}
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

{-
2. Infinite Diagonal Matrix
Construct an infinite diagonal matrix as an infinite list of lists:

[[1, 0, 0, 0, ...],
[0, 1, 0, 0, ...],
[0, 0, 1, 0, ...],
.
.
.
]
-}

infinite_diagonal :: [[Int]]
infinite_diagonal = iterate (0:) (1 : repeat 0)

{-
3. Pascal's Triangle
Recall that Pascal's triangle looks like this:

1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
. . .

Construct Pascal's triangle as an infinite list of lists:

[[1], [1, 1], [1, 2, 1], [1, 3, 3, 1], ...]
-}
nextRow :: [Integer] -> [Integer]
nextRow list = [a + b | (a, b) <- zip (0 : list) (list ++ [0])]

pascal :: [[Integer]]
pascal = iterate nextRow [1]

{-
4. Prime Sieve
Construct an infinite list of all prime numbers using the Sieve of Eratosthenes.
-}
remove_elements :: Integer -> [Integer] -> [Integer]
remove_elements _ [] = []
remove_elements x (y : ys) 
    | x == y = remove_elements x ys 
    | y `mod` x == 0 = remove_elements x ys 
    | otherwise = y : remove_elements x ys

sieve :: [Integer]
sieve = 
    let sieve_start (x:list) = x : sieve_start (remove_elements x (x:list))
    in sieve_start [2..]
    
{-
5. Fractions
Consider our generalized fraction type:

GFrac a = GFrac a a   -- numerator, denominator
Declare that the type is an instance of the Read and Show type classes.
-}
data GFrac a = GFrac a a 

instance (Eq a, Num a) => Eq (GFrac a) where 
    (GFrac a b) == (GFrac c d) = (a * d == b * c)

instance (Show a) => Show (GFrac a) where
    show (GFrac a b) = show a ++ "/" ++ show b

-- convert :: String -> GFrac a 
-- convert (x:y:z:xs) = GFrac x z 

-- instance (Read a) => Read (GFrac a) where 
--     read s = convert s

{-
6. Time
Create a Haskell type Time that represents a time of day with 1-second resolution,
such as 5:15:02 or 13:12:45. Create a function of type Int -> Int -> Int -> Time 
that constructs a Time from values h, m, and s, where 0 <= h < 24 and 0 <= m, s < 60. 
Also declare that Time is an instance of the type classes Eq, Ord, Enum, Bounded, 
Read, and Show.
-}

data Time = Time {hours :: Int, minutes :: Int, seconds :: Int} deriving (Eq, Ord)

makeTime :: Int -> Int -> Int -> Time 
makeTime = Time

instance Enum Time where 
    toEnum value = Time (value `div` 3600 `mod` 24) (value `div` 60 `mod` 60)
                        (value `div` 60)
    
    fromEnum (Time h m s) = s + (m + h * 60) * 60

instance Bounded Time where
    minBound = Time 0 0 0
    maxBound = Time 23 59 59

instance Show Time where 
    show (Time h m s) = 
        (if h > 9 then show h else "0"++show h)++":"++
        (if m > 9 then show m else "0"++show m)++":"++
        (if s > 9 then show s else "0"++show s)

{-
7. Balance Test - one of the homeworks 
-}