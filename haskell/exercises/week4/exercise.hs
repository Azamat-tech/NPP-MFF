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
data Op = Minus | Plus | Times
data Expr = Const Int | Var String | OpExpr Expr Op Expr 

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
Write a function simplify :: Exp -> Exp that simplifies an expression by combining constants wherever possible, and by eliminating additions/subtractions with 0 or multiplications with 1.

For example, 0 + (x + (2 + 3)) should simplify to x + 5.
-}