import Data.Maybe
import Data.Char
{-
1. Mapping and Filtering

Recall the signatures of the built-in functions map and filter :

map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> [a] -> [a]

Write these functions using foldl and/or foldr.
-}

map1 :: (a -> b) -> [a] -> [b] 
map1 f = foldr (\x acc -> f x : acc) []

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f = foldr (\x acc -> if f x then x : acc else acc) []

{-
2. takeWhile
takeWhile returns the longest prefix of elements that satisfy a predicate:

> takeWhile (\i -> i * i < 30) [1 ..]
[1,2,3,4,5]
What is the type of takeWhile?

Write takeWhile recursively.

Write takeWhile using foldr.
-}
--Recursive implementation
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 f (x:xs)
    | f x = x : takeWhile1 f xs
    | otherwise = []

-- using folr
takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 f = foldr(\x acc -> if f x then x : acc else []) []

{- 3. Class Discussion -}

{-
4. Tree Values - Class Work
Consider this definition of a binary tree:

data Tree a = Nil | Node (Tree a) a (Tree a)
Write a function treeVals that takes a binary search tree and returns a list of all its values in order:

treeVals :: Ord a => Tree a -> [a]
How efficient is your function?
-}
data Tree a = Nil | Node (Tree a) a (Tree a)

-- recursive approach -> O(nLogn) 
treeVals :: Ord a =>Tree a -> [a]
treeVals Nil = []
treeVals (Node left x right) = treeVals left ++ [x] ++ treeVals right

-- better approach -> linear time
treeVals1 :: Ord a => Tree a -> [a] 
treeVals1 tree = 
    let f :: Tree a -> [a] -> [a]
        f Nil a = a
        f (Node left x right) a = 
            let rightList = f right a 
            in f left (x : rightList)
    in f tree []
{-
5. Valid Search Tree
Write a function isSearchTree that determines whether a binary tree is a valid binary search tree. 
Assume that duplicate values are not allowed in the tree.
-}
isSorted :: Ord a => [a] -> Bool
isSorted [] = True 
isSorted [x] = True
isSorted (x:y:xs)
    | x < y = isSorted (y:xs)
    | otherwise = False 

-- using the previous treeVals task
isSearchTree :: Ord a => Tree a -> Bool 
isSearchTree Nil = True
isSearchTree tree = 
    let treeValues = treeVals1 tree
    in isSorted treeValues

-- other approach is to recursively check the values on left being less than node
-- same for the right subtrees

{-
6. Tree Fold 
Write a function treeFoldr that performs a right fold of a function over the 
values in a binary tree:

treeFoldr :: (b -> a -> a) -> a -> Tree b -> a
Use treeFoldr to write functions that

add all the values in a binary tree of numbers;

generate a list containing all the values in a binary tree
-}
treeFoldr :: (b -> a -> a) -> a -> Tree b -> a
treeFoldr _ a Nil = a
treeFoldr f a (Node left x right) = treeFoldr f firstBase left
    where firstBase = (f x secondBase)
          secondBase = (treeFoldr f a right)

{-
7. Algebraic Expressions - CLASS WORK
a) Design a data type Exp that can represent any arithmetic expression 
involving integer constants, named variables, and the +, - and * operators. 
Such expressions include

(x + y) * (x – y)
z * (z – 2)
14
0 + (1 + (2 + 3))
b) Write a function eval :: Exp -> [(String, Int)] -> Int that evaluates an
expression given an environment, which is an association list that holds 
the variables of the variables in the expression.
-}
data Op = Minus | Plus | Times deriving (Show)
data Expr = Const Int | Var String | OpExpr Expr Op Expr deriving (Show)

opfun :: Op -> (Int -> Int -> Int)
opfun Minus = (-)
opfun Plus = (+)
opfun Times = (*)

eval :: Expr -> [(String,Int)] -> Int 
eval expr lVars = case expr of 
    Const num -> num 
    Var v -> fromJust (lookup v lVars)
    OpExpr left op right ->
        let l = eval left lVars 
            r = eval right lVars
        in opfun op l r

{-
8. Infix Expressions - CLASS WORK
Write a function parse :: String -> Exp that can parse an expression written 
in prefix notation, such as

* + x y - x y
* z - z 2
14
+ 0 + 1 + 2 3
You may assume that (as in the examples above) all input tokens are separated 
by spaces.
-}
isOperator :: String -> Bool
isOperator l = elem l ["+","-","*"]

convert :: String -> Op
convert "+" = Plus
convert "-" = Minus
convert "*" = Times

parse :: String -> Expr 
parse s = 
    let f :: [String] -> (Expr, [String])
        f (token : rest) 
            | isDigit (head token) = (Const (read token), rest)
            | isLetter (head token) = (Var token, rest)
            | isOperator token = 
                let (left, rest1) = f rest
                    (right, rest2) = f rest1
                in (OpExpr left (convert token) right, rest2)
        (expr, rest) = f (words s)
    in expr 


{-
9. Simplifying Expressions
Write a function simplify :: Exp -> Exp that simplifies an expression by combining 
constants wherever possible, and by eliminating additions/subtractions with 0 or 
multiplications with 1.

For example, 0 + (x + (2 + 3)) should simplify to x + 5.
-}
-- simplify :: Expr -> Expr 
-- simplify OpExpr ((Const a) Plus (Const b)) = Const (a + b)


{-
10. Lists - CLASS WORK
Pretend that Haskell does not have a built-in list type.

a) Implement lists using an abstract data type DList a. Comparison operators 
(e.g. ==, /=, >, <) should work on your class and should compare lists lexicographically.

b) Write a function that computes the sum of all values in a DList Int.

c) Write a function that performs a right fold over a DList a.
-}
data DList a = Empty | Cons a (DList a) deriving (Ord, Eq, Show)

dSum :: DList Int -> Int
dSum Empty = 0 
dSum (Cons x rest) = x + dSum rest

dfoldr :: (b -> a -> a) -> a -> DList b -> a
dfoldr _ a Empty = a
dfolr f a (Cons x rest) = f x (dfoldr f a rest)

{-
11. Natural Numbers
Pretend that Haskell does not have the built-in types Int and Integer.

a) Implement natural numbers (i.e. non-negative integers) using an abstract
data type Nat. Comparison operators (e.g. ==, /=, >, <) should work correctly
on your class.

b) Implement an addition function add :: Nat -> Nat -> Nat.

c) Implement a multiplication function mul :: Nat -> Nat -> Nat.
-}
data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)

one = Succ Zero
two = Succ one
three = Succ two
four = Succ three

add :: Nat -> Nat -> Nat
add Zero x = x
add (Succ n) y = add n (Succ y)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Succ n) y = add (mul n y) y

{-
12. Pythagorean Triples
Write a function triples that takes an integer N and returns a list of integer 
triples (a, b, c) with 0 < a < b < c ≤ N such that a2 + b2 = c2. If two triples 
are multiples of each other (e.g. (3, 4, 5) and (6, 8, 10)), only the smaller of 
them (e.g. (3, 4, 5)) should appear in the list.

> triples 15

[(3,4,5),(5,12,13)]
-}
firstT :: (Int, Int, Int) -> Int
firstT (x,_,_) = x

secondT :: (Int, Int, Int) -> Int
secondT (_,x,_) = x

thirdT :: (Int, Int, Int) -> Int
thirdT (_,_,x) = x

multiple :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
multiple x y 
    | x < y && firstT y `div` firstT x == secondT y `div` secondT x && 
        secondT y `div` secondT x == thirdT y `div` thirdT x = True
    | x > y && firstT x `div` firstT y == secondT x `div` secondT y && 
        secondT x `div` secondT y == thirdT x `div` thirdT y = True
    | otherwise = False

removeMultiple :: (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int, Int)]
removeMultiple _ [] = []
removeMultiple x (y:ys) 
    | multiple x y = removeMultiple x ys 
    | otherwise = y : removeMultiple x ys
        
check :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)] 
check [] xs = xs
check (x:xs) ys = if elem x ys then check xs (removeMultiple x ys) else check xs ys

triples :: Int -> [(Int, Int, Int)]
triples n = let xs = [(a,b,c) | c <- [1..n], b <- [1..c], a <- [1..b] , a^2 + b^2 == c^2]
            in check xs xs

{-
13. Words
Write a function words :: String → [String] that breaks a string into words. 
For the purposes of this exercise, a word is any sequence of characters that 
are not spaces. Words may be separated by any number of spaces.
-}

words1 :: String -> [String]
words1 [] = []
words1 text@(char:otherWords)     
    | isSpace char = words1 otherWords
    | otherwise = let tempWord = takeWhile (/= ' ') text
                      tempOther = dropWhile (/= ' ') text
                  in tempWord : words1 tempOther
