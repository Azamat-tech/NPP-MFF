-- replicate :: Int -> a -> [a]
-- so pass the power and function to get [(+2) (+2) (+2)]
-- foldr :: (b → a → a) → a → [b] → a

pow :: Int -> (a -> a) -> (a -> a)
pow times function = 
    let replFunction = replicate times function
    in foldl(\acc f -> f . acc) id replFunction
    
add :: Int -> Int -> Int
add x y = pow x succ y

mul :: Int -> Int -> Int
mul x y = pow x (add y ) 0

iexp :: Int -> Int -> Int
iexp x y = pow y (mul x) 1