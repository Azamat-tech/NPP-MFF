import Data.Char

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y : xs) = if x < y then x : y : xs 
                    else (if x == y then y : xs else y : insert x xs)

insertion_sort :: [Int] -> [Int]
insertion_sort [x] = [x]
insertion_sort (x : xs) = insert x (insertion_sort xs)

capitalize_bigger_string :: String -> String
capitalize_bigger_string [x] = [x]
capitalize_bigger_string (x : y : xs) = if isSpace x && isLower y 
                            then x : toUpper y : capitalize_bigger_string xs
                            else  x : capitalize_bigger_string(y : xs)

capitalize :: String -> String 
capitalize [x] = [toUpper x]
capitalize (x : xs) = capitalize_bigger_string(toUpper x : xs)